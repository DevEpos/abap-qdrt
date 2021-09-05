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
    CONSTANTS:
      BEGIN OF c_uri_params,
        read_metadata TYPE string VALUE 'readMetadata',
      END OF c_uri_params.

    DATA:
      query_config     TYPE zcl_qdrt_entity_data_provider=>ty_query_config,
      is_read_metadata TYPE abap_bool.

    METHODS:
      parse_body,
      read_uri_params.
ENDCLASS.



CLASS zcl_qdrt_entity_qry_reslt_res IMPLEMENTATION.

  METHOD if_rest_resource~post.
    read_uri_params( ).
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
          data             = query_config ).
      " 2) convert filter values
      "  -> Automatic conversion to internal abap types not possible due to dynamic nature
      "     of the passed filters, e.g. Date will come as yyyy-mm-dd
    ENDIF.
  ENDMETHOD.


  METHOD read_uri_params.
    DATA uri_param_value TYPE string.
    uri_param_value = mo_request->get_uri_query_parameter(
      iv_name    = c_uri_params-read_metadata
      iv_encoded = abap_false ).

    IF uri_param_value = 'true'.
      is_read_metadata = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
