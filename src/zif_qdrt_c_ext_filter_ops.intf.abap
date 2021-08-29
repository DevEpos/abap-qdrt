"! <p class="shorttext synchronized" lang="en">Filter operations (External)</p>
INTERFACE zif_qdrt_c_ext_filter_ops
  PUBLIC .

  CONSTANTS contains TYPE string VALUE 'Contains'.
  CONSTANTS starts_with TYPE string VALUE 'StartsWith'.
  CONSTANTS ends_with TYPE string VALUE 'EndsWith'.
  CONSTANTS between TYPE string VALUE 'BT'.
  CONSTANTS equals TYPE string VALUE 'EQ'.
  CONSTANTS greater_equals TYPE string VALUE 'GE'.
  CONSTANTS greater_than TYPE string VALUE 'GT'.
  CONSTANTS lesser_equal TYPE string VALUE 'LE'.
  CONSTANTS lesser_than TYPE string VALUE 'LT'.
  CONSTANTS empty TYPE string VALUE 'Empty'.
ENDINTERFACE.
