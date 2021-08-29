*"* use this source file for your ABAP unit test classes
CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_qdrt_sql_query_parser.

    METHODS:
      setup,
      test_normal_select FOR TESTING
        RAISING cx_static_check,
      test_with_select FOR TESTING
        RAISING cx_static_check,
      test_join FOR TESTING
        RAISING cx_static_check,
      test_join2 FOR TESTING
        RAISING cx_static_check,
      test_union FOR TESTING
        RAISING cx_static_check,
      test_sub_query FOR TESTING
        RAISING cx_static_check,
      test_params FOR TESTING
        RAISING cx_static_check,
      test_no_terminator FOR TESTING
        RAISING cx_static_check,
      test_is_count_query FOR TESTING
        RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.
  METHOD setup.
  ENDMETHOD.

  METHOD test_normal_select.
    DATA: lt_query TYPE TABLE OF string.

    cut = NEW #( query = 'SELECT * FROM MARA' ).
    TRY.
        cut->parse( ).
      CATCH zcx_qdrt_appl_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_with_select.
    DATA: query_lines TYPE TABLE OF string.
    query_lines = VALUE #(
      ( |DATA: carrid type spfli-carrid.| )
      ( )
      ( |WITH | )
      ( |    +cities AS (| )
      ( |        SELECT cityfrom AS city| )
      ( |            FROM spfli| )
      ( |            WHERE carrid = @carrid | )
      ( |        UNION DISTINCT         | )
      ( |        SELECT cityto AS city          | )
      ( |            FROM spfli             | )
      ( |            WHERE carrid = @carrid | )
      ( |     )     | )
      ( |     SELECT *       | )
      ( |        FROM sgeocity      | )
      ( |        WHERE city IN ( SELECT city                | )
      ( |                            FROM +cities ).          | )
    ).

    CONCATENATE LINES OF query_lines INTO DATA(query) SEPARATED BY cl_abap_char_utilities=>cr_lf.
    cut = NEW #( query ).
    TRY.
        cut->parse( ).
      CATCH zcx_qdrt_appl_error INTO DATA(error).
    ENDTRY.

*    cl_abap_unit_assert=>assert_bound(
*        act = lx_error
*        msg = 'Unsupported WITH query not caught'
*    ).
  ENDMETHOD.

  METHOD test_join.
    DATA: lt_query TYPE TABLE OF string.

    lt_query = VALUE #(
      ( |SELECT *| )
      ( |  FROM mara AS material| )
      ( |    INNER JOIN marc AS plant| )
      ( |      ON material~matnr = plant~matnr| )
      ( |  WHERE plant like '1002'.          | )
    ).

    CONCATENATE LINES OF lt_query INTO DATA(lv_query) SEPARATED BY cl_abap_char_utilities=>cr_lf.
    cut = NEW #( lv_query ).

    TRY.
        cut->parse( ).
      CATCH zcx_qdrt_appl_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_join2.
    DATA: lt_query TYPE TABLE OF string.

    lt_query = VALUE #(
      ( |SELECT| )
      ( |  FROM /dry/process AS process| )
      ( |    LEFT OUTER JOIN /dry/proccstctr AS costcenter| )
      ( |      ON process~processuuid = costcenter~processuuid| )
      ( |    JOIN /dry/i_workcentercostcenter AS processcostcenter| )
      ( |      ON process~dairyresourceinternalid = processcostcenter~workcenterinternalid| )
      ( |  FIELDS process~dairyprocessexternalid| )
    ).

    CONCATENATE LINES OF lt_query INTO DATA(lv_query) SEPARATED BY cl_abap_char_utilities=>cr_lf.
    cut = NEW #( lv_query ).

    TRY.
        cut->parse( ).
      CATCH zcx_qdrt_appl_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_union.
    DATA: lt_query TYPE TABLE OF string.

    lt_query = VALUE #(
      ( |SELECT costcenter AS entity, CAST( 'Cost Center' AS CHAR ) AS type| )
      ( |  FROM /dry/proccstctr| )
      ( |  UNION| )
      ( |SELECT costelement AS entity, CAST( 'Cost Element' AS CHAR ) AS type| )
      ( |  FROM /dry/proccstelmt| )
      ( |  WHERE costelement LIKE '%'| )
    ).

    CONCATENATE LINES OF lt_query INTO DATA(lv_query) SEPARATED BY cl_abap_char_utilities=>cr_lf.
    cut = NEW #( lv_query ).
    TRY.
        cut->parse( ).
      CATCH zcx_qdrt_appl_error.
    ENDTRY.

  ENDMETHOD.

  METHOD test_sub_query.
    DATA: lt_query TYPE TABLE OF string.

    lt_query = VALUE #(
      ( |SELECT *| )
      ( |  FROM mara| )
      ( |  WHERE matnr IN ( SELECT matnr| )
      ( |                     FROM marc| )
      ( |                     WHERE matnr LIKE 'F%'| )
      ( |                       AND werks IN ( '1002','1003','1004' )| )
      ( |                     GROUP BY matnr| )
      ( |                 ) | )
    ).

    CONCATENATE LINES OF lt_query INTO DATA(lv_query) SEPARATED BY cl_abap_char_utilities=>cr_lf.
    cut = NEW #( lv_query ).
    TRY.
        cut->parse( ).
      CATCH zcx_qdrt_appl_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_params.
    DATA: query_lines TYPE TABLE OF string.

    query_lines = VALUE #(
      ( |DATA: p_plant value '1002' type werks_d,| )
      ( |      p_inline(4) type c value '003',| )
      ( |      p_from_date TYPE dats.| )
      ( || )
      ( |SELECT costcenter AS entity, CAST( 'Cost Center' AS CHAR ) AS type| )
      ( |  FROM /dry/proccstctr| )
      ( |  UNION| )
      ( |SELECT costelement AS entity, CAST( 'Cost Element' AS CHAR ) AS type| )
      ( |  FROM /dry/proccstelmt| )
      ( |  WHERE costelement LIKE '%'| )
    ).

    CONCATENATE LINES OF query_lines INTO DATA(query) SEPARATED BY cl_abap_char_utilities=>cr_lf.
    cut = NEW #( query ).
    TRY.
        cut->parse( ).
      CATCH zcx_qdrt_appl_error.
    ENDTRY.


  ENDMETHOD.

  METHOD test_no_terminator.
    DATA: query_lines TYPE TABLE OF string.

    query_lines = VALUE #(
      ( |SELECT matnr, mtart from mara| )
    ).

    CONCATENATE LINES OF query_lines INTO DATA(query) SEPARATED BY cl_abap_char_utilities=>cr_lf.
    cut = NEW #( query ).
    TRY.
        cut->parse( ).
      CATCH zcx_qdrt_appl_error.
    ENDTRY.

  ENDMETHOD.

  METHOD test_is_count_query.
    DATA: query_lines TYPE TABLE OF string.
    query_lines = VALUE #(
      ( |DATA: carrid type spfli-carrid.| )
      ( )
      ( |WITH | )
      ( |    +cities AS (| )
      ( |        SELECT cityfrom AS city| )
      ( |            FROM spfli| )
      ( |            WHERE carrid = @carrid | )
      ( |        UNION DISTINCT         | )
      ( |        SELECT cityto AS city          | )
      ( |            FROM spfli             | )
      ( |            WHERE carrid = @carrid | )
      ( |     )     | )
      ( |     SELECT COUNT( * ) as count       | )
      ( |        FROM sgeocity      | )
      ( |        WHERE city IN ( SELECT city                | )
      ( |                            FROM +cities ).          | )
    ).

    CONCATENATE LINES OF query_lines INTO DATA(query) SEPARATED BY cl_abap_char_utilities=>cr_lf.
    cut = NEW #( query ).
    TRY.
        DATA(query_ref) = cut->parse( ).
        cl_abap_unit_assert=>assert_true(
          act = query_ref->query_data-is_single_result_query
          msg = 'Count query could not be detected'
        ).
      CATCH zcx_qdrt_appl_error INTO DATA(error).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_bound(
        act = error
        msg = COND #( WHEN error IS BOUND THEN error->get_longtext( ) )
    ).
  ENDMETHOD.

ENDCLASS.
