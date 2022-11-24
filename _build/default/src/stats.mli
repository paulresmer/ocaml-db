open Dbtype

(*[sum c] is the sum of the values in column [c]. Raises [InvalidNumericColumn]
  if [c.col_type] is not one of [TInt] or [TFloat] *)
val sum : column -> float

(*[mean c] is the mean of the values in column [c]. Raises
  [InvalidNumericColumn] if [c.col_type] is not one of [TInt] or [TFloat] *)
val mean : column -> float

(*[max c] is the maximum of the values in column [c]. Raises
  [InvalidNumericColumn] if [c.col_type] is not one of [TInt] or [TFloat] *)
val max : column -> float

(*[min c] is the minimum of the values in column [c]. Raises
  [InvalidNumericColumn] if [c.col_type] is not one of [TInt] or [TFloat] *)
val min : column -> float

(*[median c] is the median of the values in column [c]. Raises
  [InvalidNumericColumn] if [c.col_type] is not one of [TInt] or [TFloat] *)
val median : column -> float

(*[variance c] is the variance of the values in column [c]. Raises
  [InvalidNumericColumn] if [c.col_type] is not one of [TInt] or [TFloat] *)
val variance : column -> float

(*[variance c] is the variance of the values in column [c]. Raises
  [InvalidNumericColumn] if [c.col_type] is not one of [TInt] or [TFloat] *)
val std_dev : column -> float
