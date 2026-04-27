function evaluate(str: string): bigint | { exp: bigint } {
  str = str.replace(/\s+/g, '');
  const result: Result<Expr> = exprParser.run(str);
  if (result === null || result.remainder !== '') {
    throw new SyntaxError('Failed to parse');
  }

  try {
    return { exp: resolveLog10(result.value) };
  } catch (err) {
    return resolve(result.value);
  }
}

class ExprTree {
  constructor(
    public expr1: ExprTree | bigint,
    public op: Op,
    public expr2: ExprTree | bigint
  ) {}
}

function resolve(value: ExprTree | bigint): bigint {
  if (!(value instanceof ExprTree)) return value;
  switch (value.op) {
    case '+': return resolve(value.expr1) + resolve(value.expr2);
    case '-': return resolve(value.expr1) - resolve(value.expr2);
    case '*': return resolve(value.expr1) * resolve(value.expr2);
    case '/': return resolve(value.expr1) / resolve(value.expr2);
    case '%': return resolve(value.expr1) % resolve(value.expr2);
    case '^': return resolve(value.expr1) ** resolve(value.expr2);
    default: throw new SyntaxError('Unknown operator: ' + value.op);
  }
}

function resolveLog10(value: Expr): bigint {
  if (!(value instanceof ExprTree)) {
    const s = value.toString(10);
    return BigInt(s.length + Math.log10(Number('0.' + s.substring(0, 15))));
  } else switch (value.op) {
    case '*': return resolve(new ExprTree(resolveLog10(value.expr1), '+', resolveLog10(value.expr2)));
    case '/': return resolve(new ExprTree(resolveLog10(value.expr1), '-', resolveLog10(value.expr2)));
    case '^': return resolve(new ExprTree(resolveLog10(value.expr1), '*', value.expr2));
    default: return resolveLog10(resolve(value));
  }
}

type Expr = ExprTree | bigint;
type Op = '+' | '-' | '*' | '/' | '%' | '^';

type Result<T> = { value: T, remainder: string } | null;

class Parser<T> {

  constructor(public run: (str: string) => Result<T>) {}

  static pure<T>(value: T | Parser<T>): Parser<T> {
    if (value instanceof Parser) return value;
    return new Parser(str => ({ value, remainder: str }));
  }

  static fail<T>(): Parser<T> {
    return new Parser(() => null);
  }

  static match(...values: string[]): Parser<string> {
    return new Parser(str => {
      for (const value of values) {
        if (str.startsWith(value)) {
          return { value, remainder: str.slice(value.length) }
        }
      }
      return null;
    });
  }

  static repeat<T>(min: number, parser: Parser<T>): Parser<Array<T>> {
    return parser.next(head => {
      return Parser.repeat(Math.max(0, min - 1), parser).next(tail => {
        return [head, ...tail];
      })
    }, () => {
      return min <= 0 ? Parser.pure([]) : Parser.fail();
    })
  }

  next<S>(getNextParser: (token: T) => S | Parser<S>, getFailover?: () => S | Parser<S>): Parser<S> {
    return new Parser(str => {
      const result = this.run(str);
      if (result !== null) {
        const nextParser = Parser.pure(getNextParser(result.value));
        return nextParser.run(result.remainder);
      } else if (getFailover) {
        const nextParser = Parser.pure(getFailover());
        return nextParser.run(str);
      } else {
        return null;
      }
    });
  }
}

const signParser: Parser<bigint> = Parser.match('+', '-', '').next((sign: string) => {
  switch (sign) {
    case '-': return -1n;
    default: return 1n;
  }
});

const digitsParser: Parser<bigint> = new Parser(str => {
  if (!/^[0-9]+/.test(str)) return null;
  const splits = str.split(/(?<=^[0-9]+)(?=[^0-9])/);
  return {
    value: BigInt(splits[0]),
    remainder: splits[1] || ''
  }
});

const naturalParser: Parser<bigint> = digitsParser.next((mantissa: bigint) => {
  return Parser.match('e', 'E').next(() => {
    return signParser.next((sign: bigint) => {
      return digitsParser.next(exponent => {
        return mantissa * (10n ** (exponent * sign));
      });
    });
  }, () => mantissa);
});

const op1Parser: Parser<Op> = Parser.match('^').next(op => {
  switch (op) {
    case '^': return '^';
    default: throw new Error();
  }
});

const op2Parser: Parser<Op> = Parser.match('*', '/', '%').next(op => {
  switch (op) {
    case '*': return '*';
    case '/': return '/';
    case '%': return '%';
    default: throw new Error();
  }
});

const op3Parser: Parser<Op> = Parser.match('+', '-').next(op => {
  switch (op) {
    case '+': return '+';
    case '-': return '-';
    default: throw new Error();
  }
});

const precParsers: Array<Parser<Expr>> = [
  Parser.match('(').next(() => {
    return precParsers[3].next(prec3 => {
      return Parser.match(')').next(() => {
        return prec3;
      })
    })
  }, () => naturalParser),
  Parser.match('').next(() => {
    return precParsers[0].next(expr1 => {
      return op1Parser.next(op => {
        return precParsers[1].next(expr2 => {
          return new ExprTree(expr1, op, expr2);
        });
      }, () => expr1);
    })
  }),
  Parser.match('').next(() => termsParser(op2Parser, precParsers[1])),
  Parser.match('').next(() => termsParser(op3Parser, precParsers[2]))
]

function termsParser(opParser: Parser<Op>, termParser: Parser<Expr>) {
  return termParser.next(firstTerm => {
    return Parser.repeat(0, opParser.next(op => {
      return termParser.next(term => ({ op, term }))
    })).next(terms => {
      let expr: Expr = firstTerm;
      for (const { op, term } of terms) {
        expr = new ExprTree(expr, op, term);
      }
      return expr;
    });
  })
}

const exprParser: Parser<Expr> = precParsers[3];

/*
<expr> ::= <prec3>
<prec3> ::= <prec2> <op3> <prec3> | <prec2>
<op3> ::= '+' | '-'
<prec2> ::= <prec1> <op2> <prec2> | <prec1>
<op2> ::= '*' | '/' | '%'
<prec1> ::= <prec0> '^' <prec1> | <prec0>
<prec0> ::= '(' <expr> ')' | <number>

<natural> ::= <digits> | <digits><e><sign><digits>
<sign> ::= '+' | '-' | ''
<e> ::= 'e' | 'E'
<digits> ::= <digit> | <digit><digits>
<digit> ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
*/