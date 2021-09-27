"! <p class="shorttext synchronized" lang="en">VH data provider for domains with fix values</p>
CLASS zcl_qdrt_dom_fix_values_vhdp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      zif_qdrt_vh_data_provider.
    METHODS:
      constructor
        IMPORTING
          rollname TYPE rollname.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_fix_values_vh_result,
        fix_value   TYPE string,
        description TYPE string,
      END OF ty_fix_values_vh_result,

      ty_fix_values_vh_results TYPE STANDARD TABLE OF ty_fix_values_vh_result WITH EMPTY KEY.

    DATA:
      rollname TYPE rollname.
ENDCLASS.



CLASS zcl_qdrt_dom_fix_values_vhdp IMPLEMENTATION.


  METHOD constructor.
    me->rollname = rollname.
  ENDMETHOD.


  METHOD zif_qdrt_vh_data_provider~get_data.
    DATA: fix_values_results TYPE ty_fix_values_vh_results.

    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = rollname
      RECEIVING
        p_descr_ref    = DATA(dtel_descr)
      EXCEPTIONS
        type_not_found = 1 ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_qdrt_appl_error
        EXPORTING
          text = |Rollname '{ rollname }' does not exist in the repository|.
    ELSEIF dtel_descr->kind <> cl_abap_typedescr=>kind_elem OR
        dtel_descr->is_ddic_type( ) = abap_false.
      RAISE EXCEPTION TYPE zcx_qdrt_appl_error
        EXPORTING
          text = |{ rollname } is no valid type in DDIC|.
    ENDIF.

    LOOP AT CAST cl_abap_elemdescr( dtel_descr )->get_ddic_fixed_values( ) ASSIGNING FIELD-SYMBOL(<fix_value>).
      APPEND VALUE #( fix_value = <fix_value>-low description = <fix_value>-ddtext ) TO fix_values_results.
    ENDLOOP.

    result = zcl_qdrt_json=>to_json(
      data        = fix_values_results
      compress    = abap_false " empty values are possible
      pretty_name = zcl_qdrt_json=>pretty_mode-camel_case ).
  ENDMETHOD.


ENDCLASS.
