"! <p class="shorttext synchronized" lang="en">Extension of /UI2/CL_JSON</p>
CLASS zcl_qdrt_json DEFINITION
  PUBLIC
  INHERITING FROM /ui2/cl_json
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Converts the given data object to a JSON string</p>
      to_json
        IMPORTING
          data             TYPE data
          compress         TYPE abap_bool OPTIONAL
          name             TYPE string OPTIONAL
          pretty_name      TYPE pretty_name_mode DEFAULT pretty_mode-none
          type_descr       TYPE REF TO cl_abap_typedescr OPTIONAL
          assoc_arrays     TYPE abap_bool OPTIONAL
          ts_as_iso8601    TYPE abap_bool OPTIONAL
          expand_includes  TYPE abap_bool DEFAULT abap_true
          assoc_arrays_opt TYPE abap_bool OPTIONAL
          numc_as_string   TYPE abap_bool OPTIONAL
          name_mappings    TYPE name_mappings OPTIONAL
          conversion_exits TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(result)    TYPE json,
      "! <p class="shorttext synchronized" lang="en">Converts a given JSON string to an ABAP representation</p>
      to_abap
        IMPORTING
          json             TYPE json OPTIONAL
          jsonx            TYPE xstring OPTIONAL
          pretty_name      TYPE pretty_name_mode DEFAULT pretty_mode-none
          assoc_arrays     TYPE abap_bool OPTIONAL
          assoc_arrays_opt TYPE abap_bool OPTIONAL
          name_mappings    TYPE name_mappings OPTIONAL
          conversion_exits TYPE abap_bool OPTIONAL
        CHANGING
          data             TYPE data.
  PROTECTED SECTION.
    METHODS:
      dump_type REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_qdrt_json IMPLEMENTATION.


  METHOD to_json.

    DATA(serializer) = NEW zcl_qdrt_json(
      compress         = compress
      pretty_name      = pretty_name
      name_mappings    = name_mappings
      assoc_arrays     = assoc_arrays
      assoc_arrays_opt = assoc_arrays_opt
      expand_includes  = expand_includes
      numc_as_string   = numc_as_string
      conversion_exits = conversion_exits
      ts_as_iso8601    = ts_as_iso8601 ).
    result = serializer->serialize_int(
      name       = name
      data       = data
      type_descr = type_descr ).
  ENDMETHOD.


  METHOD dump_type.
    " custom handling of convexit call. The original code does not escape characters which
    " are to be escaped in a json string
    IF data IS NOT INITIAL AND convexit IS NOT INITIAL.
      TRY.
          CALL FUNCTION convexit
            EXPORTING
              input  = data
            IMPORTING
              output = r_json
            EXCEPTIONS
              OTHERS = 1.
          IF sy-subrc IS INITIAL.
            r_json = |"{ escape( r_json ) }"|.
          ENDIF.
        CATCH cx_root.                                  "#EC NO_HANDLER
      ENDTRY.
      RETURN.
    ENDIF.

    r_json = super->dump_type(
      data       = data
      type_descr = type_descr
      convexit   = space ).
  ENDMETHOD.


  METHOD to_abap.
    IF json IS NOT INITIAL OR jsonx IS NOT INITIAL.

      TRY .
          DATA(deserializer) = NEW zcl_qdrt_json(
            pretty_name      = pretty_name
            name_mappings    = name_mappings
            assoc_arrays     = assoc_arrays
            conversion_exits = conversion_exits
            assoc_arrays_opt = assoc_arrays_opt ).
          deserializer->deserialize_int(
            EXPORTING
              json  = json
              jsonx = jsonx
            CHANGING
              data  = data ).
        CATCH cx_sy_move_cast_error.                    "#EC NO_HANDLER
      ENDTRY.

    ENDIF.
  ENDMETHOD.

ENDCLASS.
