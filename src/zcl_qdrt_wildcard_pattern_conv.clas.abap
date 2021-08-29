"! <p class="shorttext synchronized" lang="en">Wildcard pattern converter</p>
CLASS zcl_qdrt_wildcard_pattern_conv DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Convert escape characters from SAP to SQL</p>
      conv_sap_to_sql_pattern
        IMPORTING
          sap_pattern          TYPE clike
        EXPORTING
          VALUE(sql_pattern)   TYPE string
          VALUE(escape_needed) TYPE abap_bool.
  PRIVATE SECTION.
    CONSTANTS:
      c_sap_any_single   TYPE c VALUE '+',
      c_sap_any_sequence TYPE c VALUE '*',

      c_sap_escape       TYPE c VALUE '#',

      c_sql_any_single   TYPE c VALUE '_',
      c_sql_any_sequence TYPE c VALUE '%'.
ENDCLASS.

CLASS zcl_qdrt_wildcard_pattern_conv IMPLEMENTATION.


  METHOD conv_sap_to_sql_pattern.
    DATA: last_pos      TYPE i,
          curr_pos      TYPE i VALUE 0,
          continue_off  TYPE i,
          next_char     TYPE c,
          next_char_pos TYPE i.

    CLEAR sql_pattern.

    last_pos = strlen( sap_pattern ) - 1.

    WHILE curr_pos <= last_pos.
      continue_off = 1.

      CASE sap_pattern+curr_pos(1).

        WHEN c_sap_any_single.
          CONCATENATE sql_pattern c_sql_any_single INTO sql_pattern.

        WHEN c_sap_any_sequence.
          CONCATENATE sql_pattern c_sql_any_sequence INTO sql_pattern.

        WHEN c_sap_escape.

          IF curr_pos < last_pos.
*.......... character after escape is single byte
            next_char_pos = curr_pos + 1.
            ADD 1 TO continue_off.
*.......... Character after escape is sqlMeta or sapEscape
            next_char = sap_pattern+next_char_pos(1).
            IF next_char = c_sql_any_single OR
                next_char = c_sql_any_sequence OR
                next_char = c_sap_escape.
              escape_needed = abap_true.
              CONCATENATE sql_pattern c_sap_escape INTO sql_pattern.
            ENDIF.
            CONCATENATE sql_pattern next_char INTO sql_pattern.
          ENDIF.

        WHEN c_sql_any_single OR
             c_sql_any_sequence.
          escape_needed = abap_true.
          CONCATENATE sql_pattern c_sap_escape sap_pattern+curr_pos(1) INTO sql_pattern.

        WHEN OTHERS.
          CONCATENATE sql_pattern sap_pattern+curr_pos(1) INTO sql_pattern RESPECTING BLANKS.
      ENDCASE.

      curr_pos = curr_pos + continue_off.
    ENDWHILE.

  ENDMETHOD.


ENDCLASS.
