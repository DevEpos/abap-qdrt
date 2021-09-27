"! <p class="shorttext synchronized" lang="en">Rest Resource for Value Helps</p>
CLASS zcl_qdrt_entities_res DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM cl_rest_resource.

  PUBLIC SECTION.
    METHODS:
      constructor,
      if_rest_resource~get REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_entity,
        type         TYPE string,
        name         TYPE string,
        description  TYPE string,
        package_name TYPE string,
        is_favorite  TYPE abap_bool,
      END OF ty_entity.

    TYPES: BEGIN OF ty_entity_extended.
             INCLUDE TYPE ty_entity.
    TYPES alt_name TYPE ddlname.
    TYPES: END OF ty_entity_extended.

    DATA:
      sql_to_upper_in_regex     TYPE string,
      name_filter_range         TYPE RANGE OF tabname,
      descr_filter_range        TYPE RANGE OF ddtext,
      max_rows                  TYPE i,
      extended_search_result    TYPE STANDARD TABLE OF ty_entity_extended WITH EMPTY KEY,
      entity_type_range         TYPE RANGE OF zif_qdrt_ty_global=>ty_entity_type,
      entity_search_scope_range TYPE RANGE OF abap_bool.

    METHODS:
      read_uri_params,
      select_data,
      filter_cds_views,
      set_response.
ENDCLASS.



CLASS zcl_qdrt_entities_res IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    IF sy-saprl >= 751.
      sql_to_upper_in_regex = |UPPER( &1 )|.
    ELSE.
      sql_to_upper_in_regex = |&1|.
    ENDIF.
  ENDMETHOD.


  METHOD if_rest_resource~get.
    read_uri_params( ).
    select_data( ).
    filter_cds_views( ).
    set_response( ).
  ENDMETHOD.


  METHOD read_uri_params.
    DATA(top_param) = mo_request->get_uri_query_parameter( iv_name = '$top' iv_encoded = abap_false ).
    max_rows = CONV i( top_param ).
    IF max_rows IS INITIAL.
      max_rows = 200.
    ENDIF.

    DATA(name_filter) = mo_request->get_uri_query_parameter( iv_name = 'name' iv_encoded = abap_false ).
    IF name_filter IS NOT INITIAL.
      name_filter_range = VALUE #( ( sign   = 'I'
                                     option = COND #( WHEN name_filter CA '*+' THEN 'CP' ELSE 'EQ' )
                                     low    = to_upper( name_filter ) ) ).
    ENDIF.

    DATA(descr_filter) = mo_request->get_uri_query_parameter( iv_name = 'description' iv_encoded = abap_false ).
    IF descr_filter IS NOT INITIAL.
      IF sy-saprl >= 751.
        TRANSLATE descr_filter TO UPPER CASE.
      ENDIF.
      descr_filter_range = VALUE #( ( sign   = 'I'
                                      option = COND #( WHEN descr_filter CA '*+' THEN 'CP' ELSE 'EQ' )
                                      low    = descr_filter ) ).
    ENDIF.

    DATA(entity_type) = to_upper( mo_request->get_uri_query_parameter( iv_name = 'entityType' ) ).
    IF entity_type IS NOT INITIAL.
      entity_type_range = VALUE #( ( sign = 'I' option = 'EQ' low = entity_type ) ).
    ENDIF.

    DATA(search_scope) = mo_request->get_uri_query_parameter( iv_name = 'scope' ).
    IF search_scope = 'favorites'.
      entity_search_scope_range = VALUE #( ( sign = 'I' option = 'EQ' low = abap_true ) ).
    ENDIF.
  ENDMETHOD.


  METHOD select_data.

    DATA(description_dyn_where) = replace( val = sql_to_upper_in_regex sub = '&1' with = 'entity~description' ) &&
                                  | IN @descr_filter_range|.

    SELECT type,
           rawentityid AS name,
           altentityid AS alt_name,
           description,
           developmentpackage AS package_name,
           fav~isfavorite AS is_favorite
      FROM zqdrt_i_dbentity AS entity
        LEFT OUTER JOIN zqdrt_i_dbentityfavorite AS fav
          ON  entity~entityid = fav~entityid
          AND entity~type     = fav~entitytype
      WHERE entity~entityid IN @name_filter_range
        AND type IN @entity_type_range
        AND fav~isfavorite IN @entity_search_scope_range
        AND (description_dyn_where)
      ORDER BY type,
               entity~entityid
      INTO CORRESPONDING FIELDS OF TABLE @extended_search_result
      UP TO @max_rows ROWS.
  ENDMETHOD.


  METHOD filter_cds_views.
    DATA:
       ddl_sources TYPE HASHED TABLE OF zif_qdrt_ty_global=>ty_ddl_source WITH UNIQUE KEY ddlname.

    CHECK: sy-saprl >= 751,
           line_exists( extended_search_result[ type = zif_qdrt_c_entity_types=>cds_view ] ).

    DATA(proj_fields) = `ddlname, source_type`.

    SELECT (proj_fields)
      FROM ddddlsrc
      FOR ALL ENTRIES IN @extended_search_result
      WHERE ddlname = @extended_search_result-alt_name
      INTO CORRESPONDING FIELDS OF TABLE @ddl_sources.

    IF sy-subrc = 0.
      LOOP AT extended_search_result ASSIGNING FIELD-SYMBOL(<entity>) WHERE type = zif_qdrt_c_entity_types=>cds_view.
        ASSIGN ddl_sources[ ddlname = <entity>-alt_name ] TO FIELD-SYMBOL(<ddl_source>).

        IF <ddl_source>-source_type NOT IN zcl_qdrt_cds_util=>valid_ddl_source_types.
          DELETE extended_search_result.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD set_response.
    DATA:
      search_result TYPE TABLE OF ty_entity.

    IF extended_search_result IS NOT INITIAL.
      search_result = CORRESPONDING #( extended_search_result ).
      DATA(response_entity) = mo_response->create_entity( ).

      response_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
      DATA(json_str) = zcl_qdrt_json=>to_json(
        data        = search_result
        pretty_name = zcl_qdrt_json=>pretty_mode-camel_case ).
      response_entity->set_string_data( json_str ).
      mo_response->set_status( 200 ).
    ELSE.
      mo_response->set_status( 204 ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
