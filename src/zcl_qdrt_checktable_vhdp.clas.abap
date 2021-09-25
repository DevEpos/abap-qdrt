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
          checktable TYPE tabname.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      checktable TYPE tabname.
ENDCLASS.



CLASS zcl_qdrt_checktable_vhdp IMPLEMENTATION.


  METHOD constructor.
    me->checktable = checktable.
  ENDMETHOD.


  METHOD zif_qdrt_vh_data_provider~get_data.
    DATA: checktable_result TYPE REF TO data.
    FIELD-SYMBOLS: <checktable_result> TYPE STANDARD TABLE.

    DATA(struct_descr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( to_upper( checktable ) ) ).
    DATA(table_descr) = cl_abap_tabledescr=>create(
      p_line_type = struct_descr ).

    CREATE DATA checktable_result TYPE HANDLE table_descr.

    ASSIGN checktable_result->* TO <checktable_result>.

    SELECT *
      FROM (checktable)
      UP TO 200 ROWS
      INTO CORRESPONDING FIELDS OF TABLE <checktable_result>.

    IF sy-subrc = 0.
      result = /ui2/cl_json=>serialize(
        data        = <checktable_result>
        compress    = abap_true
        pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
    ENDIF.
  ENDMETHOD.


ENDCLASS.
