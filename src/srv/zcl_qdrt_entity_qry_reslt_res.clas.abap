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
      BEGIN OF c_uri_attributes,
        name TYPE string VALUE 'name',
        type TYPE string VALUE 'type',
      END OF c_uri_attributes.

    DATA:
      query_config TYPE zcl_qdrt_entity_data_provider=>ty_query_config,
      entity_name  TYPE zif_qdrt_ty_global=>ty_entity_name,
      entity_type  TYPE zif_qdrt_ty_global=>ty_entity_type,

      BEGIN OF query_result,
        results TYPE REF TO data,
        count   TYPE zqdrt_no_of_lines,
      END OF query_result,
      data_provider TYPE REF TO zcl_qdrt_entity_data_provider.

    METHODS: parse_body,
      read_uri_params
        RAISING
          zcx_qdrt_appl_error,
      execute_selection
        RAISING
          zcx_qdrt_appl_error,
      set_response.
ENDCLASS.



CLASS zcl_qdrt_entity_qry_reslt_res IMPLEMENTATION.

  METHOD if_rest_resource~post.
    TRY.
        read_uri_params( ).
        parse_body( ).
        execute_selection( ).
        set_response( ).
      CATCH zcx_qdrt_appl_error INTO DATA(appl_error).
        zcl_qdrt_rest_error_response=>create( response = mo_response )->set_body_from_exc( appl_error ).
    ENDTRY.
  ENDMETHOD.


  METHOD parse_body.
    DATA(json_body) = mo_request->get_entity( )->get_string_data( ).
    IF json_body IS NOT INITIAL.
      zcl_qdrt_json=>to_abap(
        EXPORTING
          json        = json_body
          pretty_name = zcl_qdrt_json=>pretty_mode-camel_case
        CHANGING
          data        = query_config ).
    ENDIF.
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
  ENDMETHOD.


  METHOD execute_selection.
    FIELD-SYMBOLS:
      <result_rows> TYPE ANY TABLE.

    data_provider = NEW zcl_qdrt_entity_data_provider(
      name         = entity_name
      type         = entity_type
      query_config = query_config ).

    IF query_config-settings-no_data_select = abap_false.
      query_result-results = data_provider->get_data( ).
    ENDIF.

    IF query_config-settings-determine_max_rows = abap_true.
      query_result-count = data_provider->get_max_row_count( ).
    ENDIF.

  ENDMETHOD.


  METHOD set_response.
    FIELD-SYMBOLS: <result_rows> TYPE ANY TABLE.
    DATA(json) = zcl_qdrt_json=>to_json(
      data             = query_result
*     compress         = abap_true
      pretty_name      = zcl_qdrt_json=>pretty_mode-low_case
      " TODO: make setting available
      conversion_exits = abap_true
      name_mappings    = VALUE #(
        ( abap = 'max_rows' json = 'maxRows' )
        ( abap = '_group_count' json = '$groupCount' ) ) ).

    " Replace any lonly '\r' characters
    DATA(xjson) = zcl_qdrt_json=>string_to_raw( json ).
    DATA: cr_byte    TYPE x VALUE '0D', " CR character
          space_byte TYPE x VALUE '20'. " Space character
    REPLACE ALL OCCURRENCES OF cr_byte IN xjson WITH space_byte IN BYTE MODE.


    data(response_entity) = mo_response->create_entity( ).
    response_entity->set_binary_data( xjson ).
    response_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
