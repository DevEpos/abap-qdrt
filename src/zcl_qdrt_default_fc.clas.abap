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
          filter TYPE zif_qdrt_filter_provider=>ty_filter.
ENDCLASS.



CLASS zcl_qdrt_default_fc IMPLEMENTATION.


  METHOD zif_qdrt_filter_converter~convert.

    LOOP AT filters ASSIGNING FIELD-SYMBOL(<filter>).

      DATA(field_metadata) = metadata_provider->get_field_metadata( fieldname = CONV #( <filter>-field_name ) ).
      CHECK field_metadata IS NOT INITIAL.

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

ENDCLASS.
