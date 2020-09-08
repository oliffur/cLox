#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

/*** SCANNER ***/

typedef struct {
  // Marks beginning of lexeme being scanned
  const char* start;

  // Sweeping pointer; the lexeme is the substring between `start` and `current`
  const char* current;

  // Track what line the current lexeme is on, for error reporting
  int line;
} Scanner;

Scanner scanner;

void initScanner(const char* source) {
  scanner.start = source;
  scanner.current = source;
  scanner.line = 1;
}



/*** TOKENS ***/

// Make a token of `type` between `scanner.start` and `scanner.current`
static Token makeToken(TokenType type) {
  Token token;
  token.type = type;
  token.start = scanner.start;
  token.length = (int)(scanner.current - scanner.start);
  token.line = scanner.line;

  return token;
}

// Also append errors to bytecode as Tokens
static Token errorToken(const char* message) {
  Token token;
  token.type = TOKEN_ERROR;
  // lexeme points to the error message string instead of user source code
  token.start = message;
  token.length = (int)strlen(message);
  token.line = scanner.line;

  return token;
}



/*** SWEEPING ***/

static bool isAtEnd() { return *scanner.current == '\0'; }
static char peek() { return *scanner.current; }

// Return character after `current`
static char peekNext() {
  if (isAtEnd()) return '\0';
  return scanner.current[1];
}

// Increment `current` and return previous `current`
static char advance() {
  scanner.current++;
  return scanner.current[-1];
}

// Attempts to match character; if match, advance and return true
static bool match(char expected) {
  if (isAtEnd()) return false;
  if (*scanner.current != expected) return false;

  scanner.current++;
  return true;
}



/*** CHARACTER MANIPULATION ***/

// Starting from `current`, skip all whitespace and comments.
static void skipWhitespace() {
  for (;;) {
    char c = peek();
    switch (c) {
      case ' ':
      case '\r':
      case '\t':
        advance();
        break;
      case '\n':
        scanner.line++;
        advance();
        break;
      case '/':
        if (peekNext() == '/') {
          // A comment goes until the end of the line.
          while (peek() != '\n' && !isAtEnd()) advance();
        } else {
          return;
        }
        break;
      default:
        return;
    }
  }
}

static bool isDigit(char c) { return c >= '0' && c <= '9'; }

static bool isAlpha(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}



/*** SCANNING ***/

// Parses a string (the initial " has already been parsed)
static Token string() {
  while (peek() != '"' && !isAtEnd()) {
    if (peek() == '\n') scanner.line++;
    advance();
  }

  if (isAtEnd()) return errorToken("Unterminated string.");

  // The closing quote.
  advance();
  return makeToken(TOKEN_STRING);
}

// Parses a number (int or double)
static Token number() {
  while (isDigit(peek())) advance();
  if (peek() == '.' && isDigit(peekNext())) {
    advance();  // Consume the ".".

    while (isDigit(peek())) advance();
  }
  return makeToken(TOKEN_NUMBER);
}

// Checks whether `rest` matches the substring of scanner that begins at
// `scanner.start + start` and is of length `length`
static TokenType checkKeyword(int start, int length,
    const char* rest, TokenType type) {
  if (scanner.current - scanner.start == start + length &&
      memcmp(scanner.start + start, rest, length) == 0) {
    return type;
  }

  return TOKEN_IDENTIFIER;
}
static TokenType identifierType() { 
  // Check for keywords using a trie.
  switch (scanner.start[0]) {
    case 'a': return checkKeyword(1, 2, "nd", TOKEN_AND);
    case 'c': return checkKeyword(1, 4, "lass", TOKEN_CLASS);
    case 'e': return checkKeyword(1, 3, "lse", TOKEN_ELSE);
    case 'f':
      if (scanner.current - scanner.start > 1) {
        switch (scanner.start[1]) {
          case 'a': return checkKeyword(2, 3, "lse", TOKEN_FALSE);
          case 'o': return checkKeyword(2, 1, "r", TOKEN_FOR);
          case 'u': return checkKeyword(2, 1, "n", TOKEN_FUN);
        }
      }
      break;
    case 'i': return checkKeyword(1, 1, "f", TOKEN_IF);
    case 'n': return checkKeyword(1, 2, "il", TOKEN_NIL);
    case 'o': return checkKeyword(1, 1, "r", TOKEN_OR);
    case 'p': return checkKeyword(1, 4, "rint", TOKEN_PRINT);
    case 'r': return checkKeyword(1, 5, "eturn", TOKEN_RETURN);
    case 's': return checkKeyword(1, 4, "uper", TOKEN_SUPER);
    case 't':
      if (scanner.current - scanner.start > 1) {
        switch (scanner.start[1]) {
          case 'h': return checkKeyword(2, 2, "is", TOKEN_THIS);
          case 'r': return checkKeyword(2, 2, "ue", TOKEN_TRUE);
        }
      }
      break;
    case 'v': return checkKeyword(1, 2, "ar", TOKEN_VAR);
    case 'w': return checkKeyword(1, 4, "hile", TOKEN_WHILE);
  }
  // Not a keyword; return identifier
  return TOKEN_IDENTIFIER;  
}
static Token identifier() {
  while (isAlpha(peek()) || isDigit(peek())) advance();

  return makeToken(identifierType());
}



/*** SCAN TOKEN ***/

// Whenever this function is called the scanner is starting a new token.
Token scanToken() {
  skipWhitespace();
  
  // scanner.start resets, and does not change until the scanToken() call.
  scanner.start = scanner.current;

  if (isAtEnd()) return makeToken(TOKEN_EOF);

  // Switch based on first character of lexeme:
  // - alpha: identifier
  // - numeric: number
  // - symbol: operation
  char c = advance();

  if (isAlpha(c)) return identifier();
  if (isDigit(c)) return number();

  switch (c) {
    case '(': return makeToken(TOKEN_LEFT_PAREN);
    case ')': return makeToken(TOKEN_RIGHT_PAREN);
    case '{': return makeToken(TOKEN_LEFT_BRACE);
    case '}': return makeToken(TOKEN_RIGHT_BRACE);
    case ';': return makeToken(TOKEN_SEMICOLON);
    case ',': return makeToken(TOKEN_COMMA);
    case '.': return makeToken(TOKEN_DOT);
    case '-': return makeToken(TOKEN_MINUS);
    case '+': return makeToken(TOKEN_PLUS);
    case '/': return makeToken(TOKEN_SLASH);
    case '*': return makeToken(TOKEN_STAR);
    case '!': return makeToken(match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
    case '=': return makeToken(match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
    case '<': return makeToken(match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
    case '>':
      return makeToken(match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
    case '"': return string();
  }

  return errorToken("Unexpected character.");
}