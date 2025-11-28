#include "tree_sitter/parser.h"
#include <string.h>
#include <ctype.h>

enum TokenType {
  FUNCTION_NAME,
  INTEGER,
  NUMBER,
};

// List of function names that need special handling
static const char* FUNCTION_NAMES[] = {
  "EXP", "LOG", "LOG10", "SQRT", "ABS",
  "DEXP", "DLOG", "DLOG10", "DSQRT", "DABS",
  "SIN", "COS", "TAN", "ASIN", "ACOS", "ATAN", "ATAN2",
  "DSIN", "DCOS", "DTAN", "DASIN", "DACOS", "DATAN", "DATAN2",
  "SINH", "COSH", "TANH", "ASINH", "ACOSH", "ATANH",
  "DSINH", "DCOSH", "DTANH",
  "MIN", "MAX", "MOD", "INT", "NINT", "DMOD",
  "GAMLN", "GAMMA", "LGAMMA", "PHI", "ERF", "ERFC",
  "FLOOR", "CEILING", "SIGN",
  "DBLE", "REAL", "FLOAT", "DFLOAT", "SNGL",
  "MIXEST", "MIXP", "MIXNUM",
  NULL
};

// Case-insensitive comparison
static int strcasecmp_custom(const char* a, const char* b) {
  while (*a && *b) {
    char ca = toupper((unsigned char)*a);
    char cb = toupper((unsigned char)*b);
    if (ca != cb) return ca - cb;
    a++;
    b++;
  }
  return toupper((unsigned char)*a) - toupper((unsigned char)*b);
}

// Check if word is a known function name
static int is_function_name(const char* word) {
  for (int i = 0; FUNCTION_NAMES[i] != NULL; i++) {
    if (strcasecmp_custom(word, FUNCTION_NAMES[i]) == 0) {
      return 1;
    }
  }
  return 0;
}

void* tree_sitter_nonmem_external_scanner_create() {
  return NULL;
}

void tree_sitter_nonmem_external_scanner_destroy(void* payload) {
}

unsigned tree_sitter_nonmem_external_scanner_serialize(void* payload, char* buffer) {
  return 0;
}

void tree_sitter_nonmem_external_scanner_deserialize(void* payload, const char* buffer, unsigned length) {
}

bool tree_sitter_nonmem_external_scanner_scan(
  void* payload,
  TSLexer* lexer,
  const bool* valid_symbols
) {
  // Skip whitespace including newlines
  while (lexer->lookahead == ' ' || lexer->lookahead == '\t' ||
         lexer->lookahead == '\n' || lexer->lookahead == '\r') {
    lexer->advance(lexer, true);
  }

  // Handle integer and number
  bool integer_valid = valid_symbols[INTEGER];
  bool number_valid = valid_symbols[NUMBER];

  // Check for optional sign followed by digit or decimal
  if ((integer_valid || number_valid) && (lexer->lookahead == '+' || lexer->lookahead == '-')) {
    lexer->advance(lexer, false);
    // Must be followed by digit or decimal to be a signed number
    if (!isdigit(lexer->lookahead) && lexer->lookahead != '.') {
      return false;
    }
  }

  if ((integer_valid || number_valid) && (isdigit(lexer->lookahead) || lexer->lookahead == '.')) {
    // Handle leading decimal like .5
    if (lexer->lookahead == '.') {
      if (!number_valid) return false;
      lexer->mark_end(lexer);
      lexer->advance(lexer, false);
      if (!isdigit(lexer->lookahead)) {
        return false;  // Not a number (.AND., etc)
      }
      while (isdigit(lexer->lookahead)) {
        lexer->advance(lexer, false);
      }
      lexer->mark_end(lexer);
      // Handle exponent
      if (lexer->lookahead == 'E' || lexer->lookahead == 'e' ||
          lexer->lookahead == 'D' || lexer->lookahead == 'd') {
        lexer->advance(lexer, false);
        if (lexer->lookahead == '+' || lexer->lookahead == '-') {
          lexer->advance(lexer, false);
        }
        while (isdigit(lexer->lookahead)) {
          lexer->advance(lexer, false);
        }
        lexer->mark_end(lexer);
      }
      lexer->result_symbol = NUMBER;
      return true;
    }

    // Must start with digit
    if (!isdigit(lexer->lookahead)) {
      return false;
    }

    // Read digits
    while (isdigit(lexer->lookahead)) {
      lexer->advance(lexer, false);
    }
    lexer->mark_end(lexer);

    // Check for decimal point
    bool has_decimal = false;
    if (lexer->lookahead == '.') {
      lexer->advance(lexer, false);

      if (isdigit(lexer->lookahead)) {
        // Has digits after decimal - it's a number
        if (!number_valid) {
          // Can't match as number, return integer part (already marked)
          lexer->result_symbol = INTEGER;
          return integer_valid;
        }
        has_decimal = true;
        while (isdigit(lexer->lookahead)) {
          lexer->advance(lexer, false);
        }
        lexer->mark_end(lexer);
      } else if (isalpha(lexer->lookahead)) {
        // Followed by letter - could be .AND., .EQ., etc.
        // Return the integer part only (already marked)
        lexer->result_symbol = integer_valid ? INTEGER : NUMBER;
        return integer_valid || number_valid;
      } else {
        // Trailing dot followed by operator (1.-X) - include the dot
        if (!number_valid) {
          // Can't match as number, return integer part (already marked)
          lexer->result_symbol = INTEGER;
          return integer_valid;
        }
        lexer->mark_end(lexer);
        lexer->result_symbol = NUMBER;
        return true;
      }
    }

    // Handle exponent (for both integer-only and decimal numbers)
    if (lexer->lookahead == 'E' || lexer->lookahead == 'e' ||
        lexer->lookahead == 'D' || lexer->lookahead == 'd') {
      if (!number_valid) {
        // Can't match as number, return integer part (already marked)
        lexer->result_symbol = INTEGER;
        return integer_valid;
      }
      lexer->advance(lexer, false);
      if (lexer->lookahead == '+' || lexer->lookahead == '-') {
        lexer->advance(lexer, false);
      }
      while (isdigit(lexer->lookahead)) {
        lexer->advance(lexer, false);
      }
      lexer->mark_end(lexer);
      lexer->result_symbol = NUMBER;
      return true;
    }

    // No decimal or exponent - return as integer or number
    lexer->result_symbol = (has_decimal || !integer_valid) ? NUMBER : INTEGER;
    return true;
  }

  // Handle function name
  if (valid_symbols[FUNCTION_NAME]) {
    // Check if we're at the start of an identifier
    if (!isalpha(lexer->lookahead) && lexer->lookahead != '_') {
      return false;
    }

    // Read the identifier
    char word[32];
    int len = 0;

    while (isalnum(lexer->lookahead) || lexer->lookahead == '_') {
      if (len < 31) {
        word[len++] = lexer->lookahead;
      }
      lexer->advance(lexer, false);
    }
    word[len] = '\0';

    // Check if it's a known function name
    if (!is_function_name(word)) {
      return false;
    }

    // Look ahead for '(' - this is the key part
    // Skip whitespace between function name and (
    while (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
      lexer->advance(lexer, true);
    }

    if (lexer->lookahead == '(') {
      lexer->result_symbol = FUNCTION_NAME;
      return true;
    }

    // Not followed by (, so it's an identifier, not a function name
    return false;
  }

  return false;
}
