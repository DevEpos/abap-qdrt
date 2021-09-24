"! <p class="shorttext synchronized" lang="en">Value Help Metadata for Field in Entity</p>
CLASS zcl_qdrt_entity_vh_meta_res DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      if_rest_resource~get REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_uri_attributes,
        name  TYPE string VALUE 'name',
        type  TYPE string VALUE 'type',
        field TYPE string VALUE 'field',
      END OF c_uri_attributes.

    DATA:
      entity_name TYPE zif_qdrt_ty_global=>ty_entity_name,
      entity_type TYPE zif_qdrt_ty_global=>ty_entity_type,
      field       TYPE fieldname.

    METHODS:
      read_uri_params
        RAISING
          zcx_qdrt_appl_error,
      get_vh_metadata_for_field
        RAISING
          zcx_qdrt_appl_error.
ENDCLASS.



CLASS zcl_qdrt_entity_vh_meta_res IMPLEMENTATION.


  METHOD if_rest_resource~get.
    TRY.
        read_uri_params( ).
        get_vh_metadata_for_field( ).
      CATCH zcx_qdrt_appl_error INTO DATA(rest_error).
        zcl_qdrt_rest_error_response=>create( mo_response )->set_body_from_exc( rest_error ).
    ENDTRY.
  ENDMETHOD.


  METHOD read_uri_params.
    entity_name = to_upper( mo_request->get_uri_attribute(
      iv_name    = c_uri_attributes-name
      iv_encoded = abap_false ) ).
    zcl_qdrt_rest_request_util=>check_empty_uri_attribute(
      uri_attribute = c_uri_attributes-name
      value         = entity_name ).

    entity_type = to_upper( mo_request->get_uri_attribute(
      iv_name    = c_uri_attributes-type
      iv_encoded = abap_false  ) ).
    zcl_qdrt_rest_request_util=>check_empty_uri_attribute(
      uri_attribute = c_uri_attributes-type
      value         = entity_type ).

    field = to_lower( mo_request->get_uri_query_parameter(
      iv_name    = c_uri_attributes-field
      iv_encoded = abap_false  ) ).
    zcl_qdrt_rest_request_util=>check_empty_uri_attribute(
      uri_attribute = c_uri_attributes-field
      value         = field ).
  ENDMETHOD.


  METHOD get_vh_metadata_for_field.

  ENDMETHOD.

ENDCLASS.
