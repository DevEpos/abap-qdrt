"! <p class="shorttext synchronized" lang="en">Filter operations (External)</p>
INTERFACE zif_qdrt_c_ext_filter_ops
  PUBLIC .

  CONSTANTS:
    contains       TYPE string VALUE 'Contains',
    starts_with    TYPE string VALUE 'StartsWith',
    ends_with      TYPE string VALUE 'EndsWith',
    between        TYPE string VALUE 'BT',
    equals         TYPE string VALUE 'EQ',
    greater_equals TYPE string VALUE 'GE',
    greater_than   TYPE string VALUE 'GT',
    lesser_equal   TYPE string VALUE 'LE',
    lesser_than    TYPE string VALUE 'LT',
    empty          TYPE string VALUE 'Empty'.
ENDINTERFACE.
