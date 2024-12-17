function nameInt(n: bigint): string {
  if (n < 0n) {
    return "negative " + nameInt(-n);
  } else if (n === 0n) {
    return "zero";
  } else {
    return buildIntName(split(n)) || 'error';
  }
}

function divMod(a: bigint, m: bigint): [bigint, bigint] {
  return [a / m, a % m];
}

function first<T>(list: Array<T | null>): T | null {
  for (const elem of list) {
    if (elem !== null) return elem;
  }
  return null;
}

function mconcat(...terms: Array<string | null>): string | null {
  let cat = '';
  for (const term of terms) {
    if (term === null) return null;
    cat = cat + term;
  }
  return cat;
}

function split(n: bigint): Array<bigint> {
  if (n === 0n) {
    return [];
  } else {
    const [d,m] = divMod(n, 1000n);
    const arr = split(d);
    arr.push(m);
    return arr;
  }
}

function buildIntName(ns: Array<bigint>): string | null {
  if (ns.length === 1) {
    return digit3(ns[0]);
  } else if (ns[0] === 0n) {
    ns.shift();
    return buildIntName(ns);
  } else {
    const n = ns[0];
    ns = ns.slice(1);
    const ord = ns.length - 1;
    const cur = mconcat(digit3(n), ' ' + nameLarge(BigInt(ord)));
    return mconcat(cur, first([term(ns), mconcat(', ', buildIntName(ns))]));
  }
}

function term(ns: Array<bigint>): string | null {
  if (ns.length === 1 && ns[0] === 0n) {
    return '';
  } else if (ns.length === 1) {
    return mconcat(' and ', digit2(ns[0]));
  } else if (ns.length > 1 && ns[0] === 0n) {
    return term(ns.slice(1));
  } else {
    return null;
  }
}

function digit3(n: bigint): string | null {
  const [d,m] = divMod(n, 100n);
  return first([digit2(n), mconcat(digit(d), ' hundred', term([m]))]);
}

function digit2(n: bigint): string | null {
  const m = n % 10n;
  return first([digit(n), teens(n), tens(n), mconcat(tens(n-m), ' ', digit(m))]);
}

function tens(n: bigint): string | null {
  switch (n) {
    case 20n: return 'twenty';
    case 30n: return 'thirty';
    case 40n: return 'forty';
    case 50n: return 'fifty';
    case 60n: return 'sixty';
    case 70n: return 'seventy';
    case 80n: return 'eighty';
    case 90n: return 'ninety';
    default: return null;
  }
}

function teens(n: bigint): string | null {
  switch (n) {
    case 10n: return 'ten';
    case 11n: return 'eleven';
    case 12n: return 'twelve';
    case 13n: return 'thirteen';
    case 14n: return 'fourteen';
    case 15n: return 'fifteen';
    case 16n: return 'sixteen';
    case 17n: return 'seventeen';
    case 18n: return 'eighteen';
    case 19n: return 'nineteen';
    default: return null;
  }
}

function digit(n: bigint): string | null {
  switch (n) {
    case 1n: return 'one';
    case 2n: return 'two';
    case 3n: return 'three';
    case 4n: return 'four';
    case 5n: return 'five';
    case 6n: return 'six';
    case 7n: return 'seven';
    case 8n: return 'eight';
    case 9n: return 'nine';
    default: return null;
  }
}

function nameLarge(n: bigint): string {
  if (n < 0n) {
    throw new Error('negative value');
  } else if (n === 0n) {
    return "thousand";
  } else {
    return nameLarge_(n);
  }
}

function nameLarge_(n: bigint): string {
  const pf = prefix(0, n);
  if (pf === '' && n < 1000n) {
    return assemble(buildLargeName(1, n));
  } else if (pf === '') {
    const [d,m] = divMod(n, 1000n);
    const s = nameLarge_(d);
    return s.substr(0, s.length - 2) + nameLarge_(m);
  } else {
    return pf + 'illion';
  }
}

function buildLargeName(s: number, n: bigint): Array<string> {
  if (s === 4) {
    return [];
  } else {
    const [d,m] = divMod(n, 10n);
    const arr = buildLargeName(s + 1, d);
    arr.unshift(prefix(s, m));
    return arr;
  }
}

const PREFIX_TABLE: Array<Array<string>> = [
	['n', 'm', 'b', 'tr', 'quadr', 'quint', 'sext', 'sept', 'oct', 'non'],
	['', 'un', 'duo', 'tre', 'quattuor', 'quin', 'se', 'septe', 'octo', 'nove'],
	['', 'deci', 'viginti', 'triginta', 'quadraginta', 'quinquaginta', 'sexaginta', 'septuaginta', 'octoginta', 'nonaginta'],
	['', 'centi', 'ducenti', 'trecenti', 'quadringenti', 'quingenti', 'sescenti', 'septingenti', 'octingenti', 'nongenti']
];

function prefix(n: number, k: bigint): string {
  return (PREFIX_TABLE[n] || [])[Number(k)] || '';
}

function assemble(terms: Array<string>): string {
  if (terms.length === 3) {
    return assemble([terms[0], terms[1] + terms[2]]);
  } else if (terms.length === 2) {
    const a = terms[0];
    const b = terms[1];
    const s1 = a + extra(a, b[0]) + b;
    return s1.slice(0, s1.length - 1) + 'illion';
  } else {
    throw new Error('developer error');
  }
}

function extra(s: string, c: string): string {
  if (s === 'se') {
    if ('qtv'.includes(c)) return 's';
    if ('oc'.includes(c)) return 'x';
  } else if (s === 'tre') {
    if (extra('se', c) !== '') return 's';
  } else if (s === 'nove') {
    if ('cdqst'.includes(c)) return 'n';
    if ('ov'.includes(c)) return 'm';
  } else if (s === 'septe') {
    return extra('nove', c);
  }
  return '';
}

module.exports = {
  nameInt,
  nameLarge
}
