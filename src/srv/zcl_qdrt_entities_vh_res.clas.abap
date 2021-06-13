"! <p class="shorttext synchronized" lang="en">Rest Resource for Value Helps</p>
CLASS zcl_qdrt_entities_vh_res DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM cl_rest_resource.

  PUBLIC SECTION.
    METHODS if_rest_resource~get
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_type,
        type         TYPE string,
        name         TYPE string,
        description  TYPE string,
        package_name TYPE string,
      END OF ty_type.
ENDCLASS.



CLASS zcl_qdrt_entities_vh_res IMPLEMENTATION.


  METHOD if_rest_resource~get.
    DATA: filter_ranges TYPE RANGE OF tabname,
          param         TYPE string,
          skip          TYPE i,
          top           TYPE i.

    DATA(filter) = mo_request->get_uri_query_parameter(
        iv_name    = 'filter'
    ).
    param = mo_request->get_uri_query_parameter(
        iv_name    = '$top'
    ).
    top = CONV i( param ).
    IF top IS INITIAL.
      top = 50.
    ENDIF.

    filter_ranges = VALUE #( ( sign   = 'I'
                               option = COND #( WHEN filter CA '*+' THEN 'CP' ELSE 'EQ' )
                               low    = to_upper( filter ) ) ).

    " TODO: copy necessary cds views into QDRT package
    SELECT
      FROM zsat_i_databaseentity
      FIELDS type,
             entityraw AS name,
             description,
             developmentpackage AS package_name
      WHERE entity IN @filter_ranges
      ORDER BY type,
               entity
    INTO TABLE @DATA(result)
      UP TO @top ROWS.

    IF result IS NOT INITIAL.
      DATA(response_entity) = mo_response->create_entity( ).

      response_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
      DATA(json_str) = /ui2/cl_json=>serialize(
         data             = result
         pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
      ).
      response_entity->set_binary_data( /ui2/cl_json=>string_to_raw( json_str ) ).
      mo_response->set_status( 200 ).
    ELSE.
      mo_response->set_status( 204 ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
