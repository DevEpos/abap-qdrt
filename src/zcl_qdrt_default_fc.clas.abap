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
      convert_date_filter
        CHANGING
          filter TYPE zif_qdrt_filter_provider=>ty_filter,
      conv_filter_ranges_to_int
        IMPORTING
          field_metadata TYPE zif_qdrt_ty_global=>ty_field_metadata
        CHANGING
          filter_ranges  TYPE zif_qdrt_filter_provider=>ty_filter_ranges,
      conv_filter_range_to_int
        IMPORTING
          field_metadata TYPE zif_qdrt_ty_global=>ty_field_metadata
        CHANGING
          filter_range   TYPE zif_qdrt_filter_provider=>ty_filter_range.
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
      CASE field_metadata-type.

        WHEN zif_qdrt_c_edm_types=>date.
          convert_date_filter( CHANGING filter = <filter> ).

      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD convert_date_filter.
    IF filter-value IS NOT INITIAL.
      filter-value = replace( val = filter-value sub = '-' with = space ).
    ENDIF.

    LOOP AT filter-items ASSIGNING FIELD-SYMBOL(<filter_item>).
      <filter_item>-key = replace( val = <filter_item>-key sub = '-' with = space ).
    ENDLOOP.

    LOOP AT filter-ranges ASSIGNING FIELD-SYMBOL(<filter_range>) WHERE value1 IS NOT INITIAL
                                                                    OR value2 IS NOT INITIAL.
      IF <filter_range>-value1 IS NOT INITIAL.
        <filter_range>-value1 = replace( val = <filter_range>-value1 sub = '-' with = space occ = 0 ).
      ENDIF.
      IF <filter_range>-value2 IS NOT INITIAL.
        <filter_range>-value2 = replace( val = <filter_range>-value1 sub = '-' with = space occ = 0 ).
      ENDIF.
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

ENDCLASS.
