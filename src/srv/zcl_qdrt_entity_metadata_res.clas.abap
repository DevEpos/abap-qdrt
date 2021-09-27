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
          zcx_qdrt_appl_error.
ENDCLASS.



CLASS zcl_qdrt_entity_metadata_res IMPLEMENTATION.


  METHOD if_rest_resource~get.

    TRY.
        DATA(entity_metadata) = get_entity_metadata( ).
      CATCH zcx_qdrt_appl_error INTO DATA(rest_error).
        zcl_qdrt_rest_error_response=>create( response = mo_response )->set_body_from_exc( rest_error ).
        RETURN.
    ENDTRY.

    DATA(response_body) = entity_metadata->get_metadata( ).
    IF response_body IS NOT INITIAL.
      mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
      DATA(json) = zcl_qdrt_json=>to_json(
        data             = response_body
        compress         = abap_true
        pretty_name      = zcl_qdrt_json=>pretty_mode-camel_case
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
    zcl_qdrt_rest_request_util=>check_empty_uri_attribute(
      uri_attribute = c_name_attribute
      value         = entity_name ).
    DATA(entity_type) = to_upper( mo_request->get_uri_attribute( iv_name = c_type_attribute iv_encoded = abap_false  ) ).
    zcl_qdrt_rest_request_util=>check_empty_uri_attribute(
      uri_attribute = c_type_attribute
      value         = entity_type ).

    result = zcl_qdrt_provider_factory=>create_entity_metadata(
      entity_name = CONV #( entity_name )
      entity_type = entity_type ).
  ENDMETHOD.

ENDCLASS.
