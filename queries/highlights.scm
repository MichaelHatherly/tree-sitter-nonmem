; Comments
(comment) @comment

; Record keywords ($PROBLEM, $PK, $THETA, etc.)
(record_keyword) @label

; Control flow keywords (IF, THEN, ELSE, DO, ENDIF, etc.)
; Note: Parameter ref keywords (THETA, ETA, etc.) inherit from parent node highlighting
(keyword) @keyword.control

; Option names (METHOD, MAXEVAL, FILE, etc.)
(option_name) @keyword

; Parameter references - highlight the entire node
(theta_ref) @constant
(eta_ref) @constant
(err_ref) @constant
(eps_ref) @constant
(eta_range) @constant

; Operators (+, -, *, /, .EQ., .AND., etc.)
(arithmetic_operator) @operator
(comparison_operator) @operator
(logical_operator) @operator

; Numbers
(number) @number
(integer) @number

; Strings
(string) @string
(filename) @string.special
(problem_text) @string

; Special string-like constructs
(fortran_line) @string.special
(format_spec) @string.special
(ignore_character) @string.special

; Built-in functions
(function_name) @function.builtin

; Subroutine calls
(call_statement
  (keyword)
  (identifier) @function)

; Parameter references - keyword and index highlighted separately, parens uncolored
; (keyword captured above as @type, integer captured as @number)

; Indexed access and identifiers
(indexed_access
  (identifier) @variable)
(identifier) @variable

; ADVAN/TRANS options
(advan_option) @keyword
(trans_option) @keyword

; Estimation options - keywords inside captured by general (keyword) rule
; (method_option, maxeval_option, print_option, niter_option, auto_option,
;  mapiter_option, sigdig_option, mceta_option, eonly_option, isample_option,
;  sig_option, sigl_option, nsig_option, msfo_option, tol_option)

; Boolean/flag options
(fix_option) @keyword
(same_option) @keyword
(structure_option) @keyword
(noabort_option) @keyword
(posthoc_option) @keyword
(interaction_option) @keyword
(nointer_option) @keyword
(laplacian_option) @keyword
(like_option) @keyword
(numerical_option) @keyword
(slow_option) @keyword
(twoll_option) @keyword
(unconditional_option) @keyword
(special_option) @keyword
(noslow_option) @keyword
(compress_option) @keyword
(rewind_option) @keyword

; Table options
(noprint_option) @keyword
(oneheader_option) @keyword
(noappend_option) @keyword
(firstreconly_option) @keyword
; format_option and file_option have aliased keywords

; Simulation options
(onlysim_option) @keyword
; subproblems_option has aliased keyword

; Covariance options - matrix_option has aliased keyword

; Data options - keyword inside is captured by general (keyword) rule
; (ignore_option has aliased keyword, drop_modifier and skip_modifier have aliased keyword)

; Model options
(ncomp_option) @keyword
(comp_option) @keyword

; Abbreviated record options
(deriv_option) @keyword
(declare_option) @keyword
(replace_option) @keyword

; Prior options
(nwpri_option) @keyword
(tnpri_option) @keyword
; ntheta_option and neta_option have aliased keywords
(values_option) @keyword

; Sizes options
(sizes_setting) @keyword
