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
      search_help         TYPE shlp_descr,
      sh_result_table     TYPE REF TO data,
      sh_return_table     TYPE TABLE OF ddshretval,
      sh_result_tab_comps TYPE abap_component_tab.

    FIELD-SYMBOLS: <sh_results> TYPE STANDARD TABLE.

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

    " create result table from search help field definition
    LOOP AT search_help-fieldprop ASSIGNING FIELD-SYMBOL(<sh_field_prop>) WHERE shlplispos > 0
                                                                             OR shlpoutput = abap_true.
      ASSIGN search_help-fielddescr[ fieldname = <sh_field_prop>-fieldname ] TO FIELD-SYMBOL(<sh_field_descr>).
      CHECK sy-subrc = 0.

      sh_result_tab_comps = VALUE #( BASE sh_result_tab_comps
        ( name = <sh_field_descr>-fieldname
          type = CAST #(
            cl_abap_typedescr=>describe_by_name( <sh_field_descr>-rollname ) ) ) ).

    ENDLOOP.
    DATA(sh_result_structdescr) = cl_abap_structdescr=>create( p_components = sh_result_tab_comps ).
    DATA(sh_result_tabdescr) = cl_abap_tabledescr=>create( p_line_type = sh_result_structdescr ).

    CREATE DATA sh_result_table TYPE HANDLE sh_result_tabdescr.
    ASSIGN sh_result_table->* TO <sh_results>.


    CALL FUNCTION 'F4IF_SELECT_VALUES'
      EXPORTING
        shlp           = search_help
        maxrows        = 200
        call_shlp_exit = abap_true
*  IMPORTING
*       maxrows_exceeded =
      TABLES
        return_tab     = sh_return_table.
    DATA(last_index) = 0.
    LOOP AT sh_return_table ASSIGNING FIELD-SYMBOL(<sh_return_field>).
      IF last_index <> <sh_return_field>-recordpos.
        last_index = <sh_return_field>-recordpos.
        APPEND INITIAL LINE TO <sh_results> ASSIGNING FIELD-SYMBOL(<sh_result_line>).
      ENDIF.

      IF <sh_result_line> IS ASSIGNED.
        ASSIGN COMPONENT <sh_return_field>-fieldname OF STRUCTURE <sh_result_line> TO FIELD-SYMBOL(<result_field>).
        IF sy-subrc = 0.
          <result_field> = <sh_return_field>-fieldval.
        ENDIF.
      ENDIF.
    ENDLOOP.

    result = /ui2/cl_json=>serialize(
      data        = <sh_results>
      compress    = abap_true
      pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
  ENDMETHOD.


ENDCLASS.
