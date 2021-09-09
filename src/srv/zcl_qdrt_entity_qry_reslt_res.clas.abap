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
      END OF c_uri_params,

      BEGIN OF c_uri_attributes,
        name TYPE string VALUE 'name',
        type TYPE string VALUE 'type',
      END OF c_uri_attributes.

    DATA:
      query_config     TYPE zcl_qdrt_entity_data_provider=>ty_query_config,
      entity_name      TYPE zif_qdrt_ty_global=>ty_entity_name,
      entity_type      TYPE zif_qdrt_ty_global=>ty_entity_type,
      is_read_metadata TYPE abap_bool.

    METHODS:
      parse_body,
      read_uri_params
        RAISING
          zcx_qdrt_rest_error.
ENDCLASS.



CLASS zcl_qdrt_entity_qry_reslt_res IMPLEMENTATION.

  METHOD if_rest_resource~post.

    TRY.
        read_uri_params( ).
      CATCH zcx_qdrt_rest_error INTO DATA(rest_error).
        mo_response->set_status( rest_error->status ).
        mo_response->set_reason( rest_error->reason ).
        RETURN.
    ENDTRY.

    parse_body( ).

    TRY.
        DATA(data_provider) = NEW zcl_qdrt_entity_data_provider(
          name         = entity_name
          type         = entity_type
          query_config = query_config ).
        DATA(query_result) = data_provider->get_data( ).
      CATCH zcx_qdrt_appl_error.
    ENDTRY.

    mo_response->set_status( 200 ).
  ENDMETHOD.


  METHOD parse_body.
    DATA(json_body) = mo_request->get_entity( )->get_string_data( ).
    IF json_body IS NOT INITIAL.
      /ui2/cl_json=>deserialize(
        EXPORTING
          json        = json_body
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
        CHANGING
          data        = query_config ).
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

    entity_name = to_upper( mo_request->get_uri_attribute(
      iv_name    = c_uri_attributes-name
      iv_encoded = abap_false ) ).
    zcl_qdrt_rest_req_util=>check_empty_uri_attribute(
      uri_attribute = c_uri_attributes-name
      value         = entity_name ).

    entity_type = to_upper( mo_request->get_uri_attribute(
      iv_name    = c_uri_attributes-type
      iv_encoded = abap_false  ) ).
    zcl_qdrt_rest_req_util=>check_empty_uri_attribute(
      uri_attribute = c_uri_attributes-type
      value         = entity_type ).
  ENDMETHOD.

ENDCLASS.
