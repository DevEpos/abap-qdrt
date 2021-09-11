"! <p class="shorttext synchronized" lang="en">Resource for entity metadata</p>
CLASS zcl_qdrt_entity_metadata_res DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      if_rest_resource~get
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      c_name_attribute TYPE string VALUE 'name',
      c_type_attribute TYPE string VALUE 'type'.

    METHODS:
      get_entity_metadata
        RETURNING
          VALUE(result) TYPE REF TO zif_qdrt_entity_metadata_prov
        RAISING
          zcx_qdrt_rest_error.
ENDCLASS.



CLASS zcl_qdrt_entity_metadata_res IMPLEMENTATION.


  METHOD if_rest_resource~get.

    TRY.
        DATA(entity_metadata) = get_entity_metadata( ).
      CATCH zcx_qdrt_rest_error INTO DATA(rest_error).
        mo_response->set_status( rest_error->status ).
        mo_response->set_reason( rest_error->reason ).
        RETURN.
    ENDTRY.

    DATA(response_body) = entity_metadata->get_metadata( ).
    IF response_body IS NOT INITIAL.
      mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
      DATA(json) = /ui2/cl_json=>serialize(
        data             = response_body
        compress         = abap_true
        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
        name_mappings    = VALUE #(
          ( abap = 'unit_field' json = 'sap:unit' )
          ( abap = 'semantics'  json = 'sap:semantics' ) ) ).
      mo_response->create_entity( )->set_string_data( json ).
    ELSE.
      mo_response->set_status( cl_rest_status_code=>gc_success_no_content ).
    ENDIF.
  ENDMETHOD.


  METHOD get_entity_metadata.
    DATA(entity_name) = to_upper( mo_request->get_uri_attribute( iv_name = c_name_attribute iv_encoded = abap_false ) ).
    zcl_qdrt_rest_req_util=>check_empty_uri_attribute(
      uri_attribute = c_name_attribute
      value         = entity_name ).
    DATA(entity_type) = to_upper( mo_request->get_uri_attribute( iv_name = c_type_attribute iv_encoded = abap_false  ) ).
    zcl_qdrt_rest_req_util=>check_empty_uri_attribute(
      uri_attribute = c_type_attribute
      value         = entity_type ).

    result = zcl_qdrt_provider_factory=>create_entity_metadata(
      entity_name = CONV #( entity_name )
      entity_type = entity_type ).

    IF result IS INITIAL.
      RAISE EXCEPTION TYPE zcx_qdrt_rest_error
        EXPORTING
          status = cl_rest_status_code=>gc_client_error_not_found
          reason = |No metadata provider found for type '{ entity_type }'|.
    ENDIF.

    IF result->entity_exists( ) = abap_false.
      RAISE EXCEPTION TYPE zcx_qdrt_rest_error
        EXPORTING
          status = cl_rest_status_code=>gc_client_error_not_found
          reason = |Entity with type '{ entity_type }' and name '{ entity_name }' does not exist|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
