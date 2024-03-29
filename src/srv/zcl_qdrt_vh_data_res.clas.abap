"! <p class="shorttext synchronized" lang="en">Resource for retrieving data of VH request</p>
CLASS zcl_qdrt_vh_data_res DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      if_rest_resource~post REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      vh_request TYPE zif_qdrt_vh_data_provider=>ty_vh_request.
ENDCLASS.



CLASS zcl_qdrt_vh_data_res IMPLEMENTATION.


  METHOD if_rest_resource~post.
    DATA(json_body) = mo_request->get_entity( )->get_string_data( ).
    IF json_body IS NOT INITIAL.
      zcl_qdrt_json=>to_abap(
        EXPORTING
          json        = json_body
          pretty_name = zcl_qdrt_json=>pretty_mode-camel_case
        CHANGING
          data        = vh_request ).
    ENDIF.

    IF vh_request-type IS INITIAL OR
        vh_request-value_help_name IS INITIAL.
      RETURN.
    ENDIF.

    TRANSLATE vh_request-value_help_name TO UPPER CASE.

    TRY.
        DATA(vh_provider) = zcl_qdrt_provider_factory=>create_vh_data_provider( vh_request ).
        DATA(vh_selection_result) = vh_provider->get_data( ).
        mo_response->create_entity( )->set_string_data(
          COND #( WHEN vh_selection_result IS NOT INITIAL THEN vh_selection_result ELSE '[]' ) ).
      CATCH zcx_qdrt_appl_error INTO DATA(appl_error).
        zcl_qdrt_rest_error_response=>create( mo_response )->set_status( )->set_body_from_exc( appl_error ).
    ENDTRY.
  ENDMETHOD.


ENDCLASS.
