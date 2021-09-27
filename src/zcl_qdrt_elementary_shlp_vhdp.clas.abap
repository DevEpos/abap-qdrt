"! <p class="shorttext synchronized" lang="en">VH data provider for elementary search help</p>
CLASS zcl_qdrt_elementary_shlp_vhdp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      zif_qdrt_vh_data_provider.

    METHODS:
      constructor
        IMPORTING
          value_help_name TYPE shlpname.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      value_help_name TYPE shlpname.
ENDCLASS.



CLASS zcl_qdrt_elementary_shlp_vhdp IMPLEMENTATION.


  METHOD constructor.
    me->value_help_name = value_help_name.
  ENDMETHOD.


  METHOD zif_qdrt_vh_data_provider~get_data.
    DATA:
      search_help TYPE shlp_descr,
      vh_result   TYPE REF TO data.

    FIELD-SYMBOLS: <vh_result> TYPE STANDARD TABLE.

    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = value_help_name
        shlptype = 'SH'
      IMPORTING
        shlp     = search_help.

    IF search_help-interface IS INITIAL.
      RAISE EXCEPTION TYPE zcx_qdrt_appl_error
        EXPORTING
          text = |No elementary search help for '{ value_help_name }' exists|.
    ENDIF.

    vh_result = zcl_qdrt_vh_util=>create_result_table_for_vh( search_help ).
    zcl_qdrt_vh_util=>select_data_via_vh(
      vh        = search_help
      vh_result = vh_result ).

    IF vh_result IS BOUND.
      ASSIGN vh_result->* TO <vh_result>.

      result = zcl_qdrt_json=>to_json(
        data             = <vh_result>
        compress         = abap_true
        conversion_exits = abap_true
        pretty_name      = zcl_qdrt_json=>pretty_mode-low_case ).
    ENDIF.
  ENDMETHOD.


ENDCLASS.
