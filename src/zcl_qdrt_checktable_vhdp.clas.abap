"! <p class="shorttext synchronized" lang="en">VH data provider for checktable search help</p>
CLASS zcl_qdrt_checktable_vhdp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_qdrt_vh_data_provider.

    METHODS:
      constructor
        IMPORTING
          checktable   TYPE tabname
          source_tab   TYPE tabname OPTIONAL
          source_field TYPE fieldname OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      checktable   TYPE tabname,
      vh_result    TYPE REF TO data,
      source_tab   TYPE tabname,
      vh_descr     TYPE shlp_descr,
      source_field TYPE fieldname.
    METHODS retrieve_shlp_by_source_tab.
    METHODS select_data_by_vh_api.
    METHODS select_data_by_sql.
ENDCLASS.



CLASS zcl_qdrt_checktable_vhdp IMPLEMENTATION.


  METHOD constructor.
    me->checktable = checktable.
    me->source_tab = source_tab.
    me->source_field = source_field.
  ENDMETHOD.


  METHOD zif_qdrt_vh_data_provider~get_data.
    FIELD-SYMBOLS: <vh_result> TYPE STANDARD TABLE.

    IF source_tab IS NOT INITIAL.
      retrieve_shlp_by_source_tab( ).
    ENDIF.

    IF vh_descr-shlpname IS NOT INITIAL.
      select_data_by_vh_api( ).
    ELSE.
      select_data_by_sql( ).
    ENDIF.

    IF vh_result IS BOUND.
      ASSIGN vh_result->* TO <vh_result>.
      result = zcl_qdrt_json=>to_json(
        data             = <vh_result>
        compress         = abap_true
        conversion_exits = abap_true
        pretty_name      = zcl_qdrt_json=>pretty_mode-low_case ).
    ENDIF.
  ENDMETHOD.



  METHOD retrieve_shlp_by_source_tab.
    CALL FUNCTION 'F4IF_DETERMINE_SEARCHHELP'
      EXPORTING
        tabname           = source_tab
        fieldname         = source_field
      IMPORTING
        shlp              = vh_descr
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        OTHERS            = 4.
  ENDMETHOD.


  METHOD select_data_by_vh_api.
    vh_result = zcl_qdrt_vh_util=>create_result_table_for_vh( vh_descr ).
    zcl_qdrt_vh_util=>select_data_via_vh(
      vh        = vh_descr
      vh_result = vh_result ).
  ENDMETHOD.


  METHOD select_data_by_sql.
    FIELD-SYMBOLS: <vh_result> TYPE STANDARD TABLE.

    DATA(struct_descr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( to_upper( checktable ) ) ).
    DATA(table_descr) = cl_abap_tabledescr=>create(
      p_line_type = struct_descr ).

    CREATE DATA vh_result TYPE HANDLE table_descr.

    ASSIGN vh_result->* TO <vh_result>.

    SELECT *
      FROM (checktable)
      UP TO 200 ROWS
      INTO CORRESPONDING FIELDS OF TABLE <vh_result>.
  ENDMETHOD.

ENDCLASS.
