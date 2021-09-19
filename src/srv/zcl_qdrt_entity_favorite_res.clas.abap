"! <p class="shorttext synchronized" lang="en">Resource for managing favorites</p>
CLASS zcl_qdrt_entity_favorite_res DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      if_rest_resource~post REDEFINITION,
      if_rest_resource~delete REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_uri_attributes,
        name TYPE string VALUE 'name',
        type TYPE string VALUE 'type',
      END OF c_uri_attributes.

    DATA:
      entity_name TYPE zif_qdrt_ty_global=>ty_entity_name,
      entity_type TYPE zif_qdrt_ty_global=>ty_entity_type.

    METHODS:
      read_uri_params
        RAISING
          zcx_qdrt_rest_error.
ENDCLASS.



CLASS zcl_qdrt_entity_favorite_res IMPLEMENTATION.


  METHOD if_rest_resource~delete.
    TRY.
        read_uri_params( ).
        DELETE FROM zqdrt_dbentf WHERE entity_name = entity_name
                                   AND entity_type = entity_type
                                   AND created_by = sy-uname.
        IF sy-subrc = 0.
          mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
          COMMIT WORK.
        ELSE.
          mo_response->set_status( cl_rest_status_code=>gc_client_error_not_found ).
          mo_response->set_reason(
            |No Favorite for Entity: { entity_name }, Type: { entity_type }, User: { sy-uname } found| ).
        ENDIF.
      CATCH zcx_qdrt_rest_error INTO DATA(rest_error).
        mo_response->set_status( rest_error->status ).
        mo_response->set_reason( rest_error->reason ).
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD if_rest_resource~post.
    TRY.
        read_uri_params( ).
        DATA(new_fav) = VALUE zqdrt_dbentf(
          entity_name = entity_name
          entity_type = entity_type
          created_by  = sy-uname ).
        INSERT zqdrt_dbentf FROM new_fav.
        IF sy-subrc = 0.
          mo_response->set_status( cl_rest_status_code=>gc_success_created ).
          COMMIT WORK.
        ELSE.
          mo_response->set_status( cl_rest_status_code=>gc_client_error_conflict ).
          mo_response->set_reason(
            |Favorite for Entity: { entity_name }, Type: { entity_type }, User: { sy-uname } already exists| ).
        ENDIF.
      CATCH zcx_qdrt_rest_error INTO DATA(rest_error).
        mo_response->set_status( rest_error->status ).
        mo_response->set_reason( rest_error->reason ).
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD read_uri_params.
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
