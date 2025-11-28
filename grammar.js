/**
 * @file NONMEM grammar for tree-sitter
 * @author Michael Hatherly
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

// Helper for comma-separated lists with at least one element
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}

// Helper for record keywords with multiple valid abbreviations
function recordKeyword(...forms) {
  return new RegExp('\\$(' + forms.join('|') + ')', 'i');
}

module.exports = grammar({
  name: "nonmem",

  externals: $ => [
    $.function_name,
    $.integer,
    $.number,
  ],

  extras: $ => [
    /\s/,
    $.comment,
    /&\r?\n/,
  ],

  rules: {
    source_file: $ => repeat($._record),

    _record: $ => choice(
      $.problem_record,
      $.input_record,
      $.data_record,
      $.subroutines_record,
      $.model_record,
      $.pk_record,
      $.des_record,
      $.error_record,
      $.pred_record,
      $.theta_record,
      $.omega_record,
      $.sigma_record,
      $.estimation_record,
      $.covariance_record,
      $.table_record,
      $.simulation_record,
      $.abbreviated_record,
      $.sizes_record,
      $.mix_record,
      $.msfi_record,
      $.nonparametric_record,
      $.prior_record,
      $.thetap_record,
      $.thetapv_record,
      $.omegap_record,
      $.omegapd_record,
      $.sigmap_record,
      $.sigmapd_record,
      $.scatter_record,
    ),

    problem_record: $ => seq(
      alias(recordKeyword('PROB', 'PROBLEM'), $.record_keyword),
      optional($.problem_text),
    ),

    problem_text: $ => /[^\n$]+/,

    input_record: $ => seq(
      alias(recordKeyword('INP', 'INPT', 'INPUT'), $.record_keyword),
      repeat($.column_def),
    ),

    column_def: $ => seq(
      field('name', $.identifier),
      optional(field('modifier', choice(
        $.drop_modifier,
        $.skip_modifier,
        $.column_alias,
      ))),
    ),

    drop_modifier: $ => seq('=', alias(/DROP/i, $.option_name)),
    skip_modifier: $ => seq('=', alias(/SKIP/i, $.option_name)),
    column_alias: $ => seq('=', field('alias', $.identifier)),

    data_record: $ => seq(
      alias(recordKeyword('DAT', 'DATA', 'INFILE'), $.record_keyword),
      optional(choice($.filename, $.string)),
      repeat($._data_option),
    ),

    _data_option: $ => choice(
      $.ignore_option,
      $.accept_option,
      $.rewind_option,
      $.records_option,
      $.null_option,
      $.wide_option,
      $.checkout_option,
    ),

    rewind_option: $ => /REWIND/i,
    records_option: $ => seq(alias(/RECORDS/i, $.option_name), optional('='), $.integer),
    null_option: $ => seq(alias(/NULL/i, $.option_name), optional('='), choice($.number, $.identifier)),
    wide_option: $ => /WIDE/i,
    checkout_option: $ => /CHECKOUT/i,

    ignore_option: $ => seq(
      alias(/(IGN|IGNORE)/i, $.option_name),
      optional('='),
      choice($.ignore_character, $.ignore_condition),
    ),

    accept_option: $ => seq(
      alias(/ACCEPT/i, $.option_name),
      optional('='),
      $.ignore_condition,
    ),

    ignore_character: $ => /[^\s\(\)=]/,
    ignore_condition: $ => seq(
      '(',
      sep1($.binary_expression, ','),
      ')'
    ),

    subroutines_record: $ => seq(
      alias(recordKeyword('SUB', 'SUBR', 'SUBS', 'SUBROUTINE', 'SUBROUTINES'), $.record_keyword),
      repeat($._subroutines_option),
    ),

    _subroutines_option: $ => choice(
      $.advan_option,
      $.trans_option,
      $.tol_option,
    ),

    advan_option: $ => choice(
      /ADVAN\d+/i,
      seq(/ADVAN/i, '=', field('number', $.integer)),
    ),
    trans_option: $ => choice(
      /TRANS\d+/i,
      seq(/TRANS/i, '=', field('number', $.integer)),
    ),
    tol_option: $ => seq(alias(/TOL/i, $.option_name), optional('='), $.integer),

    model_record: $ => seq(
      alias(recordKeyword('MOD', 'MODEL'), $.record_keyword),
      repeat($._model_option),
    ),

    _model_option: $ => choice(
      $.comp_def,
      $.ncomp_option,
    ),

    comp_def: $ => seq(
      alias(/COMP(ARTMENT)?/i, $.option_name),
      optional('='),
      choice(
        seq('(', field('name', $.identifier), repeat(seq(optional(','), $.comp_option)), ')'),
        field('name', $.identifier),
      ),
    ),

    comp_option: $ => choice(
      /DEFDOSE/i,
      /DEFOBS/i,
      /DEFOBSERVATION/i,
      /NODOSE/i,
      /INITIALOFF/i,
      /NOOFF/i,
    ),
    ncomp_option: $ => seq(alias(/NCOMP(ARTMENTS)?/i, $.option_name), optional('='), $.integer),

    pk_record: $ => seq(
      alias(recordKeyword('PK'), $.record_keyword),
      optional($.record_options),
      repeat($._statement),
    ),

    record_options: $ => seq(
      '(',
      repeat1($._record_option_item),
      ')'
    ),

    _record_option_item: $ => choice(
      $.option_setting,
      ',',
      $.identifier,
    ),

    option_setting: $ => seq(
      field('name', $.identifier),
      '=',
      field('value', choice($.number, $.integer, $.identifier)),
    ),

    des_record: $ => seq(
      alias(recordKeyword('DES'), $.record_keyword),
      optional($.record_options),
      repeat($._statement),
    ),

    error_record: $ => seq(
      alias(recordKeyword('ERR', 'ERRO', 'ERROR'), $.record_keyword),
      optional($.record_options),
      repeat($._statement),
    ),

    pred_record: $ => seq(
      alias(recordKeyword('PRED'), $.record_keyword),
      optional($.record_options),
      repeat($._statement),
    ),

    _statement: $ => choice(
      $.assignment,
      $.if_statement,
      $.block_if,
      $.do_loop,
      $.do_while_loop,
      $.exit_statement,
      $.call_statement,
      $.fortran_line,
    ),

    fortran_line: $ => /"[^\n]*/,

    if_statement: $ => seq(
      alias(/IF/i, $.keyword),
      '(',
      field('condition', $._expression),
      ')',
      field('action', $._statement),
    ),

    block_if: $ => seq(
      alias(/IF/i, $.keyword),
      '(',
      field('condition', $._expression),
      ')',
      alias(/THEN/i, $.keyword),
      field('body', $.statement_block),
      repeat($.elseif_clause),
      optional($.else_clause),
      alias(/END\s*IF/i, $.keyword),
    ),

    statement_block: $ => repeat1($._statement),

    elseif_clause: $ => seq(
      alias(/(ELSEIF|ELSE[ \t]+IF)/i, $.keyword),
      '(',
      field('condition', $._expression),
      ')',
      alias(/THEN/i, $.keyword),
      field('body', $.statement_block),
    ),

    else_clause: $ => seq(
      alias(/ELSE/i, $.keyword),
      field('body', $.statement_block),
    ),

    do_loop: $ => seq(
      alias(/DO/i, $.keyword),
      field('variable', $.identifier),
      '=',
      field('start', $._expression),
      ',',
      field('end', $._expression),
      optional(seq(',', field('step', $._expression))),
      field('body', $.statement_block),
      alias(/END\s*DO/i, $.keyword),
    ),

    do_while_loop: $ => seq(
      alias(/DO/i, $.keyword),
      alias(/WHILE/i, $.keyword),
      '(',
      field('condition', $._expression),
      ')',
      field('body', $.statement_block),
      alias(/END\s*DO/i, $.keyword),
    ),

    call_statement: $ => seq(
      alias(/CALL/i, $.keyword),
      field('name', $.identifier),
      optional(seq('(', optional(field('arguments', $.argument_list)), ')')),
    ),

    argument_list: $ => seq($._expression, repeat(seq(',', $._expression))),

    exit_statement: $ => seq(
      alias(/EXIT/i, $.keyword),
      optional(seq(field('code', $.integer), optional(field('level', $.integer)))),
    ),

    assignment: $ => seq(
      field('target', choice($.identifier, $.indexed_access)),
      '=',
      field('value', $._expression),
    ),

    _expression: $ => choice(
      $.binary_expression,
      $.unary_expression,
      $.parenthesized_expression,
      $.function_call,
      $.theta_ref,
      $.eta_ref,
      $.err_ref,
      $.eps_ref,
      $.indexed_access,
      $.identifier,
      $.number,
    ),

    binary_expression: $ => choice(
      // Exponentiation (highest, right-associative)
      prec.right(4, seq(field('left', $._expression), field('operator', alias('**', $.arithmetic_operator)), field('right', $._expression))),
      // Multiplication/Division
      prec.left(3, seq(field('left', $._expression), field('operator', alias('*', $.arithmetic_operator)), field('right', $._expression))),
      prec.left(3, seq(field('left', $._expression), field('operator', alias('/', $.arithmetic_operator)), field('right', $._expression))),
      // Addition/Subtraction
      prec.left(2, seq(field('left', $._expression), field('operator', alias('+', $.arithmetic_operator)), field('right', $._expression))),
      prec.left(2, seq(field('left', $._expression), field('operator', alias('-', $.arithmetic_operator)), field('right', $._expression))),
      // Comparisons (Fortran and C-style)
      prec.left(1, seq(field('left', $._expression), field('operator', alias(token(prec(1, /\.EQ\./i)), $.comparison_operator)), field('right', $._expression))),
      prec.left(1, seq(field('left', $._expression), field('operator', alias(token(prec(1, /\.NE\./i)), $.comparison_operator)), field('right', $._expression))),
      prec.left(1, seq(field('left', $._expression), field('operator', alias(token(prec(1, /\.GT\./i)), $.comparison_operator)), field('right', $._expression))),
      prec.left(1, seq(field('left', $._expression), field('operator', alias(token(prec(1, /\.LT\./i)), $.comparison_operator)), field('right', $._expression))),
      prec.left(1, seq(field('left', $._expression), field('operator', alias(token(prec(1, /\.GE\./i)), $.comparison_operator)), field('right', $._expression))),
      prec.left(1, seq(field('left', $._expression), field('operator', alias(token(prec(1, /\.LE\./i)), $.comparison_operator)), field('right', $._expression))),
      prec.left(1, seq(field('left', $._expression), field('operator', alias('==', $.comparison_operator)), field('right', $._expression))),
      prec.left(1, seq(field('left', $._expression), field('operator', alias('/=', $.comparison_operator)), field('right', $._expression))),
      prec.left(1, seq(field('left', $._expression), field('operator', alias('>', $.comparison_operator)), field('right', $._expression))),
      prec.left(1, seq(field('left', $._expression), field('operator', alias('<', $.comparison_operator)), field('right', $._expression))),
      prec.left(1, seq(field('left', $._expression), field('operator', alias('>=', $.comparison_operator)), field('right', $._expression))),
      prec.left(1, seq(field('left', $._expression), field('operator', alias('<=', $.comparison_operator)), field('right', $._expression))),
      // Logical (lowest)
      prec.left(0, seq(field('left', $._expression), field('operator', alias(token(prec(1, /\.AND\./i)), $.logical_operator)), field('right', $._expression))),
      prec.left(0, seq(field('left', $._expression), field('operator', alias(token(prec(1, /\.OR\./i)), $.logical_operator)), field('right', $._expression))),
    ),

    unary_expression: $ => prec.right(5, choice(
      seq(field('operator', alias('-', $.arithmetic_operator)), field('operand', $._expression)),
      seq(field('operator', alias(/\.NOT\./i, $.logical_operator)), field('operand', $._expression)),
    )),

    parenthesized_expression: $ => seq('(', $._expression, ')'),

    // External scanner handles function_name with lookahead for (
    // This allows SIN(x) as function_call but SIN alone as identifier
    function_call: $ => seq(
      field('name', $.function_name),
      '(',
      optional(field('arguments', $.argument_list)),
      ')',
    ),

    theta_ref: $ => seq(/THETA/i, '(', field('index', choice($.integer, $.identifier)), ')'),
    eta_ref: $ => seq(/ETA/i, '(', field('index', choice($.integer, $.identifier)), ')'),
    err_ref: $ => seq(/ERR/i, '(', field('index', choice($.integer, $.identifier)), ')'),
    eps_ref: $ => seq(/EPS/i, '(', field('index', choice($.integer, $.identifier)), ')'),

    indexed_access: $ => seq(
      field('name', $.identifier),
      '(',
      field('indices', $.index_list),
      ')',
    ),

    index_list: $ => seq($._expression, repeat(seq(',', $._expression))),

    theta_record: $ => seq(
      alias(recordKeyword('THE', 'THET', 'THTA', 'THETA'), $.record_keyword),
      repeat($.theta_init),
    ),

    theta_init: $ => seq(
      field('value', choice($.number, $.theta_bounds)),
      optional(field('fixed', $.fix_option)),
    ),

    theta_bounds: $ => seq(
      '(',
      choice(
        // (lower, init, upper)
        seq(
          field('lower', $._theta_bound_value),
          optional(','),
          field('init', $._theta_bound_value),
          optional(','),
          field('upper', $._theta_bound_value),
        ),
        // (lower, init)
        seq(
          field('lower', $._theta_bound_value),
          optional(','),
          field('init', $._theta_bound_value),
        ),
        // (init) only
        field('init', $._theta_bound_value),
      ),
      // FIX inside parens still valid syntax but captured at theta_init level
      optional($.fix_option),
      ')',
    ),

    _theta_bound_value: $ => choice($.number, $.identifier, $.positive_infinity, $.negative_infinity),

    positive_infinity: $ => /INF/i,
    negative_infinity: $ => /-INF/i,

    omega_record: $ => seq(
      alias(recordKeyword('OME', 'OMEG', 'OMEGA'), $.record_keyword),
      repeat($._omega_item),
    ),

    _omega_item: $ => choice(
      $.omega_init,
      $.block_spec,
      $.diagonal_spec,
      $.same_option,
      $.structure_option,
    ),

    omega_init: $ => seq(
      field('value', choice($.number, seq('(', $.number, optional($.fix_option), ')'))),
      optional(field('fixed', $.fix_option)),
    ),

    sigma_record: $ => seq(
      alias(recordKeyword('SIG', 'SIGM', 'SIGMA'), $.record_keyword),
      repeat($._sigma_item),
    ),

    _sigma_item: $ => choice(
      $.sigma_init,
      $.block_spec,
      $.diagonal_spec,
      $.same_option,
      $.structure_option,
    ),

    sigma_init: $ => seq(
      field('value', choice($.number, seq('(', $.number, optional($.fix_option), ')'))),
      optional(field('fixed', $.fix_option)),
    ),

    block_spec: $ => seq(alias(/BLOCK/i, $.option_name), '(', field('size', $.integer), ')'),
    diagonal_spec: $ => seq(alias(/DIAGONAL/i, $.option_name), '(', field('size', $.integer), ')'),
    fix_option: $ => /FIX(ED)?/i,
    same_option: $ => /SAME/i,
    structure_option: $ => choice(
      /STANDARD/i,
      /VARIANCE/i,
      /COVARIANCE/i,
      /CORRELAT(ION)?/i,
      /CHOLESKY/i,
      /SD/i,
      /VAR/i,
    ),

    estimation_record: $ => seq(
      alias(recordKeyword('EST', 'ESTI', 'ESTIM', 'ESTIMATION'), $.record_keyword),
      repeat($._estimation_option),
    ),

    _estimation_option: $ => choice(
      $.method_option,
      $.maxeval_option,
      $.print_option,
      $.noabort_option,
      $.posthoc_option,
      $.interaction_option,
      $.laplacian_option,
      $.niter_option,
      $.nburn_option,
      $.isample_option,
      $.sig_option,
      $.sigl_option,
      $.sigdig_option,
      $.nsig_option,
      $.like_option,
      $.numerical_option,
      $.slow_option,
      $.nothetaboundtest_option,
      $.noomegaboundtest_option,
      $.nosigmaboundtest_option,
      $.eonly_option,
      $.seed_option,
      $.ctype_option,
      $.citer_option,
      $.calpha_option,
      $.msfo_option,
      $.mceta_option,
      $.twoll_option,
      $.nointer_option,
      $.format_option,
      $.auto_option,
      $.mapiter_option,
      $.file_option,
      $.fnleta_option,
      $.atol_option,
      $.rtol_option,
      $.centering_option,
      $.etabar_option,
      $.df_option,
    ),

    msfo_option: $ => seq(alias(/MSFO/i, $.option_name), optional('='), $.filename),
    mceta_option: $ => seq(alias(/MCETA/i, $.option_name), optional('='), $.integer),
    twoll_option: $ => /-2LL/i,
    auto_option: $ => seq(alias(/AUTO/i, $.option_name), optional('='), choice($.integer, $.identifier)),
    mapiter_option: $ => seq(alias(/MAPITER/i, $.option_name), optional('='), $.integer),

    method_option: $ => seq(alias(/(METHOD|METH)/i, $.option_name), optional('='), choice($.integer, /COND(ITIONAL)?/i, /SAEM/i, /IMP/i, /ITS/i, /BAYES/i)),
    maxeval_option: $ => seq(alias(/(MAX|MAXEVALS?)/i, $.option_name), optional('='), $.integer),
    print_option: $ => seq(alias(/PRINT/i, $.option_name), optional('='), choice($.integer, $.identifier)),
    noabort_option: $ => /(NOAB|NOABORT)/i,
    posthoc_option: $ => /POSTHOC/i,
    interaction_option: $ => /INTER(ACTION)?/i,
    nointer_option: $ => /NOINTER(ACTION)?/i,
    laplacian_option: $ => choice(/LAPLACE/i, /LAPLACIAN/i),
    niter_option: $ => seq(alias(/NITER/i, $.option_name), optional('='), $.integer),
    nburn_option: $ => seq(alias(/NBURN/i, $.option_name), optional('='), $.integer),
    isample_option: $ => seq(alias(/ISAMPLE/i, $.option_name), optional('='), $.integer),
    sig_option: $ => seq(alias(/SIG/i, $.option_name), optional('='), $.integer),
    sigl_option: $ => seq(alias(/SIGL/i, $.option_name), optional('='), $.integer),
    sigdig_option: $ => seq(alias(/SIGDIG(ITS)?/i, $.option_name), optional('='), $.integer),
    nsig_option: $ => seq(alias(/NSIG/i, $.option_name), optional('='), $.integer),
    like_option: $ => /LIKE/i,
    numerical_option: $ => /NUMERICAL/i,
    slow_option: $ => /SLOW/i,
    nothetaboundtest_option: $ => /NOTHETABOUNDTEST/i,
    noomegaboundtest_option: $ => /NOOMEGABOUNDTEST/i,
    nosigmaboundtest_option: $ => /NOSIGMABOUNDTEST/i,
    eonly_option: $ => seq(alias(/EONLY/i, $.option_name), optional('='), $.integer),
    seed_option: $ => seq(alias(/SEED/i, $.option_name), optional('='), $.integer),
    ctype_option: $ => seq(alias(/CTYPE/i, $.option_name), optional('='), $.integer),
    citer_option: $ => seq(alias(/CITER/i, $.option_name), optional('='), $.integer),
    calpha_option: $ => seq(alias(/CALPHA/i, $.option_name), optional('='), $.number),
    fnleta_option: $ => seq(alias(/FNLETA/i, $.option_name), optional('='), $.integer),
    atol_option: $ => seq(alias(/ATOL/i, $.option_name), optional('='), $.number),
    rtol_option: $ => seq(alias(/RTOL/i, $.option_name), optional('='), $.number),
    centering_option: $ => choice(/CENTERING/i, /CENTER/i),
    etabar_option: $ => /ETABAR/i,
    df_option: $ => seq(alias(/DF/i, $.option_name), optional('='), $.integer),

    covariance_record: $ => seq(
      alias(recordKeyword('COV', 'COVA', 'COVAR', 'COVARIANCE'), $.record_keyword),
      repeat($._covariance_option),
    ),

    _covariance_option: $ => choice(
      $.print_option,
      $.unconditional_option,
      $.matrix_option,
      $.sigl_option,
      $.tol_option,
      $.special_option,
      $.slow_option,
      $.noslow_option,
      $.compress_option,
    ),

    unconditional_option: $ => /UNCONDITIONAL/i,
    matrix_option: $ => seq(alias(/MATRIX/i, $.option_name), optional('='), $.identifier),
    special_option: $ => /SPECIAL/i,
    noslow_option: $ => /NOSLOW/i,
    compress_option: $ => /COMPRESS/i,

    table_record: $ => seq(
      alias(recordKeyword('TAB', 'TABL', 'TABLE'), $.record_keyword),
      repeat($._table_item),
    ),

    _table_item: $ => choice(
      $.table_column,
      $.file_option,
      $.noprint_option,
      $.oneheader_option,
      $.noappend_option,
      $.firstreconly_option,
      $.format_option,
      $.forward_option,
      $.append_option,
      $.conditional_option,
      $.esample_option,
      $.seed_option,
      $.wreschol_option,
    ),

    table_column: $ => choice(
      $.identifier,
      $.eta_ref,
      $.eps_ref,
      $.eta_range,
    ),

    eta_range: $ => seq(/ETAS?/i, '(', field('start', $.integer), ':', field('end', $.integer), ')'),

    file_option: $ => seq(alias(/FILE/i, $.option_name), optional('='), $.filename),
    noprint_option: $ => /NOPRINT/i,
    oneheader_option: $ => /ONEHEAD(ER)?/i,
    noappend_option: $ => /NOAPP(END)?/i,
    firstreconly_option: $ => /FIRST(REC)?ONLY/i,
    format_option: $ => seq(alias(/FORMAT/i, $.option_name), optional('='), $.format_spec),
    format_spec: $ => /[^\s$]+/,
    forward_option: $ => /FORWARD/i,
    append_option: $ => /APPEND/i,
    conditional_option: $ => /CONDITIONAL/i,
    esample_option: $ => seq(alias(/ESAMPLE/i, $.option_name), optional('='), $.integer),
    wreschol_option: $ => /WRESCHOL/i,

    simulation_record: $ => seq(
      alias(recordKeyword('SIM', 'SIMU', 'SIMULATION'), $.record_keyword),
      repeat($._simulation_option),
    ),

    abbreviated_record: $ => seq(
      alias(recordKeyword('ABB', 'ABBR', 'ABBREVIATED'), $.record_keyword),
      repeat($._abbreviated_option),
    ),

    _abbreviated_option: $ => choice(
      $.replace_option,
      $.deriv_option,
      $.declare_option,
      $.comres_option,
      $.protect_option,
    ),

    replace_option: $ => seq(
      alias(/REPLACE/i, $.option_name),
      choice($.identifier, $.indexed_access),
      '=',
      $._expression
    ),
    deriv_option: $ => seq(alias(/DERIV[12]/i, $.option_name), '=', field('value', choice(/YES/i, /NO/i))),
    declare_option: $ => seq(
      alias(/DECLARE/i, $.option_name),
      optional(alias(/INTEGER|REAL|DOUBLE\s+PRECISION|DOUBLEP/i, $.option_name)),
      repeat1(choice(
        seq($.identifier, '(', field('size', $.integer), ')'),
        $.identifier
      ))
    ),
    comres_option: $ => seq(alias(/COMRES/i, $.option_name), '=', $.integer),
    protect_option: $ => alias(/PROTECT/i, $.option_name),

    sizes_record: $ => seq(
      alias(recordKeyword('SIZ', 'SIZE', 'SIZES'), $.record_keyword),
      repeat($.sizes_setting),
    ),

    sizes_setting: $ => seq(
      field('name', alias(/[A-Z][A-Z0-9]*/i, $.identifier)),
      '=',
      field('value', $.integer),
    ),

    mix_record: $ => seq(
      alias(recordKeyword('MIX'), $.record_keyword),
      repeat($._statement),
    ),

    msfi_record: $ => seq(
      alias(recordKeyword('MSF', 'MSFI'), $.record_keyword),
      optional($.filename),
    ),

    nonparametric_record: $ => seq(
      alias(recordKeyword('NONP', 'NONPARAMETRIC'), $.record_keyword),
      repeat($._nonparametric_option),
    ),

    _nonparametric_option: $ => choice(
      $.unconditional_option,
    ),

    // Bayesian prior records
    prior_record: $ => seq(
      alias(recordKeyword('PRI', 'PRIO', 'PRIOR'), $.record_keyword),
      repeat($._prior_option),
    ),

    _prior_option: $ => choice(
      $.nwpri_option,
      $.tnpri_option,
      $.ntheta_option,
      $.neta_option,
      $.nthp_option,
      $.netp_option,
      $.plev_option,
      $.ivar_option,
    ),

    nwpri_option: $ => /NWPRI/i,
    tnpri_option: $ => /TNPRI/i,
    ntheta_option: $ => seq(alias(/NTHETA/i, $.option_name), optional('='), $.integer),
    neta_option: $ => seq(alias(/NETA/i, $.option_name), optional('='), $.integer),
    nthp_option: $ => seq(alias(/NTHP/i, $.option_name), optional('='), $.integer),
    netp_option: $ => seq(alias(/NETP/i, $.option_name), optional('='), $.integer),
    plev_option: $ => seq(alias(/PLEV/i, $.option_name), optional('='), $.number),
    ivar_option: $ => seq(alias(/IVAR/i, $.option_name), optional('='), $.integer),

    thetap_record: $ => seq(
      alias(recordKeyword('THETAP'), $.record_keyword),
      repeat($.theta_init),
    ),

    thetapv_record: $ => seq(
      alias(recordKeyword('THETAPV'), $.record_keyword),
      optional(choice($.block_spec, $.diagonal_spec)),
      optional($.fix_option),
      repeat(choice($.omega_init, $.values_option)),
    ),

    values_option: $ => seq(alias(/VALUES/i, $.option_name), '(', field('value1', $.number), ',', field('value2', $.number), ')'),

    omegap_record: $ => seq(
      alias(recordKeyword('OMEGAP'), $.record_keyword),
      optional(choice($.block_spec, $.diagonal_spec)),
      optional($.fix_option),
      repeat($.omega_init),
    ),

    omegapd_record: $ => seq(
      alias(recordKeyword('OMEGAPD'), $.record_keyword),
      repeat($.omega_init),
    ),

    sigmap_record: $ => seq(
      alias(recordKeyword('SIGMAP'), $.record_keyword),
      optional(choice($.block_spec, $.diagonal_spec)),
      optional($.fix_option),
      repeat($.sigma_init),
    ),

    sigmapd_record: $ => seq(
      alias(recordKeyword('SIGMAPD'), $.record_keyword),
      repeat($.sigma_init),
    ),

    scatter_record: $ => seq(
      alias(recordKeyword('SCAT', 'SCATTER', 'SCATTERPLOT'), $.record_keyword),
      repeat($.identifier),
    ),

    _simulation_option: $ => choice(
      $.sim_seed,
      $.onlysim_option,
      $.subproblems_option,
      $.true_option,
    ),

    sim_seed: $ => seq('(', field('value', $.integer), repeat($.sim_seed_option), ')'),
    sim_seed_option: $ => choice(
      /UNIFORM/i,
      /NORMAL/i,
      /NEW/i,
    ),
    onlysim_option: $ => /ONLYSIM(ULATION)?/i,
    subproblems_option: $ => seq(alias(/SUBPROBLEMS?/i, $.option_name), optional('='), $.integer),
    true_option: $ => seq(alias(/TRUE/i, $.option_name), optional('='), /FINAL|PRIOR|INITIAL/i),

    comment: $ => /;+[^\n]*/,

    identifier: $ => /[A-Za-z_][A-Za-z0-9_]*/,
    filename: $ => /[A-Za-z0-9_.\-\/\\:~]+/,
    string: $ => choice(
      /'[^']*'/,
      /"[^"]*"/,
    ),

    // integer and number are handled by external scanner
  }
});
