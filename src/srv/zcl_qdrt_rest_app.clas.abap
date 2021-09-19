"! <p class="shorttext synchronized" lang="en">Rest Service Handler for Quick Data Reporting Tool</p>
CLASS zcl_qdrt_rest_app DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_http_handler
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      if_rest_application~get_root_handler
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_qdrt_rest_app IMPLEMENTATION.

  METHOD if_rest_application~get_root_handler.
    DATA(router) = NEW cl_rest_router( ).

    router->attach(
      iv_template      = '/'
      iv_handler_class = 'ZCL_QDRT_REST_INFO_RES' ).

    router->attach(
      iv_template      = '/entities'
      iv_handler_class = 'ZCL_QDRT_ENTITIES_RES' ).

    router->attach(
      iv_template      = '/entities/{type}/{name}/queryResult'
      iv_handler_class = 'ZCL_QDRT_ENTITY_QRY_RESLT_RES' ).

    router->attach(
      iv_template      = '/entities/{type}/{name}/metadata'
      iv_handler_class = 'ZCL_QDRT_ENTITY_METADATA_RES' ).

    router->attach(
      iv_template      = '/entities/{type}/{name}/variants'
      iv_handler_class = 'ZCL_QDRT_ENTITY_VARIANT_RES' ).

    router->attach(
      iv_template      = '/entities/{type}/{name}/variants/{variant}'
      iv_handler_class = 'ZCL_QDRT_ENTITY_VARIANT_RES' ).

    router->attach(
      iv_template      = '/entities/{type}/{name}/favorite'
      iv_handler_class = 'ZCL_QDRT_ENTITY_FAVORITE_RES' ).

    ro_root_handler = router.
  ENDMETHOD.

ENDCLASS.
