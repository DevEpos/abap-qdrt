"! <p class="shorttext synchronized" lang="en">Default Filter Converter</p>
CLASS zcl_qdrt_default_fc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      zif_qdrt_filter_converter.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:
      conv_filter_ranges_to_int
        IMPORTING
          field_metadata TYPE zif_qdrt_ty_global=>ty_field_metadata
        CHANGING
          filter_ranges  TYPE zif_qdrt_filter_provider=>ty_filter_ranges,
      conv_filter_range_to_int
        IMPORTING
          field_metadata TYPE zif_qdrt_ty_global=>ty_field_metadata
        CHANGING
          filter_range   TYPE zif_qdrt_filter_provider=>ty_filter_range,
      convert_filter_values_to_int
        IMPORTING
          field_metadata TYPE zif_qdrt_ty_global=>ty_field_metadata
        CHANGING
          filter         TYPE zif_qdrt_filter_provider=>ty_filter,
      convert_filter_value_to_int
        IMPORTING
          field_metadata TYPE zif_qdrt_ty_global=>ty_field_metadata
        CHANGING
          value          TYPE string,
      convert_date_filter
        CHANGING
          value TYPE string,
      convert_boolean_filter
        CHANGING
          filter TYPE zif_qdrt_filter_provider=>ty_filter,
      convert_time_filter
        CHANGING
          value TYPE string.
ENDCLASS.



CLASS zcl_qdrt_default_fc IMPLEMENTATION.


  METHOD zif_qdrt_filter_converter~convert.

    LOOP AT filters ASSIGNING FIELD-SYMBOL(<filter>).

      DATA(field_metadata) = metadata_provider->get_field_metadata( fieldname = CONV #( <filter>-field_name ) ).
      CHECK field_metadata IS NOT INITIAL.

      conv_filter_ranges_to_int(
        EXPORTING
          field_metadata = field_metadata
        CHANGING
          filter_ranges  = <filter>-ranges ).

      " check if conversion to some internal representation is needed
      convert_filter_values_to_int(
        EXPORTING
          field_metadata = field_metadata
        CHANGING
          filter         = <filter> ).
    ENDLOOP.

  ENDMETHOD.


  METHOD conv_filter_ranges_to_int.

    LOOP AT filter_ranges ASSIGNING FIELD-SYMBOL(<filter_range>).
      conv_filter_range_to_int(
        EXPORTING
          field_metadata = field_metadata
        CHANGING
          filter_range   = <filter_range> ).

      IF <filter_range>-operation IS INITIAL.
        DELETE filter_ranges.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD conv_filter_range_to_int.
    DATA:
      contains_regex_pattern TYPE string.

    CASE filter_range-operation.

      WHEN zif_qdrt_c_ext_filter_ops=>contains OR
           zif_qdrt_c_ext_filter_ops=>starts_with OR
           zif_qdrt_c_ext_filter_ops=>ends_with.

        CASE filter_range-operation.
          WHEN zif_qdrt_c_ext_filter_ops=>contains.
            contains_regex_pattern = `*&1*`.

          WHEN zif_qdrt_c_ext_filter_ops=>starts_with.
            contains_regex_pattern = `&1*`.

          WHEN zif_qdrt_c_ext_filter_ops=>ends_with.
            contains_regex_pattern = `*&1`.
        ENDCASE.

        filter_range-operation = zif_qdrt_c_filter_ops=>contains_pattern.
        filter_range-value1 = replace( val = contains_regex_pattern sub = '&1' with = filter_range-value1 ).

      WHEN zif_qdrt_c_ext_filter_ops=>empty.
        filter_range-operation = zif_qdrt_c_filter_ops=>equals.
        CLEAR: filter_range-value1,
               filter_range-value2.
        " TODO: Some data types like RAW need another default value than ''

      WHEN zif_qdrt_c_ext_filter_ops=>between OR
           zif_qdrt_c_ext_filter_ops=>equals OR
           zif_qdrt_c_ext_filter_ops=>greater_equals OR
           zif_qdrt_c_ext_filter_ops=>greater_than OR
           zif_qdrt_c_ext_filter_ops=>lesser_equal OR
           zif_qdrt_c_ext_filter_ops=>lesser_than.
      WHEN OTHERS.
        CLEAR filter_range-operation.
    ENDCASE.

  ENDMETHOD.


  METHOD convert_filter_values_to_int.

    CASE field_metadata-type.

      WHEN zif_qdrt_c_edm_types=>boolean.
        convert_boolean_filter(
          CHANGING
            filter = filter ).

      WHEN OTHERS.

        IF filter-value IS NOT INITIAL.
          convert_filter_value_to_int(
            EXPORTING
              field_metadata = field_metadata
            CHANGING
              value          = filter-value ).
        ENDIF.

        LOOP AT filter-items ASSIGNING FIELD-SYMBOL(<filter_item>).
          convert_filter_value_to_int(
            EXPORTING
              field_metadata = field_metadata
            CHANGING
              value          = <filter_item>-key ).
        ENDLOOP.

        LOOP AT filter-ranges ASSIGNING FIELD-SYMBOL(<filter_range>) WHERE value1 IS NOT INITIAL
                                                                        OR value2 IS NOT INITIAL.
          IF <filter_range>-value1 IS NOT INITIAL.
            convert_filter_value_to_int(
              EXPORTING
                field_metadata = field_metadata
              CHANGING
                value          = <filter_range>-value1 ).

          ENDIF.
          IF <filter_range>-value2 IS NOT INITIAL.
            convert_filter_value_to_int(
              EXPORTING
                field_metadata = field_metadata
              CHANGING
                value          = <filter_range>-value2 ).
          ENDIF.
        ENDLOOP.

    ENDCASE.
  ENDMETHOD.


  METHOD convert_filter_value_to_int.
    CASE field_metadata-type.

      WHEN zif_qdrt_c_edm_types=>date.
        convert_date_filter( CHANGING value = value ).

      WHEN zif_qdrt_c_edm_types=>time.
        convert_time_filter( CHANGING value = value ).

    ENDCASE.
  ENDMETHOD.


  METHOD convert_date_filter.
    value = replace( val = value sub = '-' with = space occ = 0 ).
  ENDMETHOD.


  METHOD convert_boolean_filter.

    CLEAR: filter-ranges,
           filter-items.

    IF filter-value = 'true'.
      filter-items = VALUE #( ( key = abap_true ) ).
    ELSE.
      filter-items = VALUE #( ( key = abap_false ) ).
    ENDIF.

    CLEAR filter-value.

  ENDMETHOD.


  METHOD convert_time_filter.
    value = replace( val = value sub = ':' with = space occ = 0 ).
  ENDMETHOD.

ENDCLASS.
