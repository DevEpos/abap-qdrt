"! <p class="shorttext synchronized" lang="en">Resource for retrieving db entity query result</p>
CLASS zcl_qdrt_entity_qry_reslt_res DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS if_rest_resource~post
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_query_request,
        filters TYPE zif_qdrt_filter_provider=>ty_filters,
      END OF ty_query_request.

    DATA:
      parsed_body TYPE ty_query_request.

    METHODS:
      parse_body.
ENDCLASS.



CLASS zcl_qdrt_entity_qry_reslt_res IMPLEMENTATION.

  METHOD if_rest_resource~post.
    " 1) parse body
    parse_body( ).
    mo_response->set_status( 200 ).
  ENDMETHOD.


  METHOD parse_body.
    DATA(json_body) = mo_request->get_entity( )->get_string_data( ).
    IF json_body IS NOT INITIAL.
      /ui2/cl_json=>deserialize(
        EXPORTING
          json             = json_body
          pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
        CHANGING
          data             = parsed_body ).
      " 2) convert filter values
      "  -> Automatic conversion to internal abap types not possible due to dynamic nature
      "     of the passed filters, e.g. Date will come as yyyy-mm-dd
    ENDIF.
  ENDMETHOD.

ENDCLASS.
