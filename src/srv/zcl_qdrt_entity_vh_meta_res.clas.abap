"! <p class="shorttext synchronized" lang="en">Resource for Value Help Metadata</p>
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
        name            TYPE string VALUE 'name',
        type            TYPE string VALUE 'type',
        field           TYPE string VALUE 'field',
        field_type      TYPE string VALUE 'fieldType',
        value_help_type TYPE string VALUE 'valueHelpType',
      END OF c_uri_attributes.

    DATA:
      entity_name     TYPE zif_qdrt_ty_global=>ty_entity_name,
      entity_type     TYPE zif_qdrt_ty_global=>ty_entity_type,
      field           TYPE fieldname,
      field_type      TYPE zif_qdrt_ty_global=>ty_field_type,
      value_help_type TYPE string.

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

    field = to_upper( mo_request->get_uri_query_parameter(
      iv_name    = c_uri_attributes-field
      iv_encoded = abap_false  ) ).
    zcl_qdrt_rest_request_util=>check_empty_uri_attribute(
      uri_attribute = c_uri_attributes-field
      value         = field ).

    field_type = mo_request->get_uri_query_parameter(
      iv_name    = c_uri_attributes-field_type
      iv_encoded = abap_false  ).
    zcl_qdrt_rest_request_util=>check_empty_uri_attribute(
      uri_attribute = c_uri_attributes-field_type
      value         = field_type ).

    value_help_type = mo_request->get_uri_query_parameter(
      iv_name    = c_uri_attributes-value_help_type
      iv_encoded = abap_false  ).
    zcl_qdrt_rest_request_util=>check_empty_uri_attribute(
      uri_attribute = c_uri_attributes-value_help_type
      value         = value_help_type ).
  ENDMETHOD.


  METHOD get_vh_metadata_for_field.
    DATA(vh_metadata_provider) = zcl_qdrt_provider_factory=>create_field_vh_metadata_prov(
      entity_name     = entity_name
      entity_type     = entity_type
      fieldname       = field
      field_type      = field_type
      value_help_type = value_help_type ).

    DATA(metadata) = vh_metadata_provider->get_metadata( ).
    mo_response->create_entity( )->set_string_data(
      zcl_qdrt_json=>to_json(
        data        = metadata
        compress    = abap_true
        pretty_name = zcl_qdrt_json=>pretty_mode-camel_case ) ).
  ENDMETHOD.

ENDCLASS.
