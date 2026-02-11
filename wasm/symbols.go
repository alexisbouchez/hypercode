//go:build js && wasm

package main

import (
	"errors"
	"fmt"
	"math"
	"math/rand"
	"reflect"
	"sort"
	"strconv"
	"strings"

	"go/constant"
	"go/token"
)

// Symbols contains only the standard library packages needed for the lessons.
// This is a minimal subset of github.com/traefik/yaegi/stdlib to reduce WASM size.
var Symbols = map[string]map[string]reflect.Value{}

func init() {
	// fmt
	Symbols["fmt/fmt"] = map[string]reflect.Value{
		"Append":       reflect.ValueOf(fmt.Append),
		"Appendf":      reflect.ValueOf(fmt.Appendf),
		"Appendln":     reflect.ValueOf(fmt.Appendln),
		"Errorf":       reflect.ValueOf(fmt.Errorf),
		"Fprint":       reflect.ValueOf(fmt.Fprint),
		"Fprintf":      reflect.ValueOf(fmt.Fprintf),
		"Fprintln":     reflect.ValueOf(fmt.Fprintln),
		"Fscan":        reflect.ValueOf(fmt.Fscan),
		"Fscanf":       reflect.ValueOf(fmt.Fscanf),
		"Fscanln":      reflect.ValueOf(fmt.Fscanln),
		"Print":        reflect.ValueOf(fmt.Print),
		"Printf":       reflect.ValueOf(fmt.Printf),
		"Println":      reflect.ValueOf(fmt.Println),
		"Scan":         reflect.ValueOf(fmt.Scan),
		"Scanf":        reflect.ValueOf(fmt.Scanf),
		"Scanln":       reflect.ValueOf(fmt.Scanln),
		"Sprint":       reflect.ValueOf(fmt.Sprint),
		"Sprintf":      reflect.ValueOf(fmt.Sprintf),
		"Sprintln":     reflect.ValueOf(fmt.Sprintln),
		"Sscan":        reflect.ValueOf(fmt.Sscan),
		"Sscanf":       reflect.ValueOf(fmt.Sscanf),
		"Sscanln":      reflect.ValueOf(fmt.Sscanln),
		"Formatter":    reflect.ValueOf((*fmt.Formatter)(nil)),
		"GoStringer":   reflect.ValueOf((*fmt.GoStringer)(nil)),
		"ScanState":    reflect.ValueOf((*fmt.ScanState)(nil)),
		"Scanner":      reflect.ValueOf((*fmt.Scanner)(nil)),
		"State":        reflect.ValueOf((*fmt.State)(nil)),
		"Stringer":     reflect.ValueOf((*fmt.Stringer)(nil)),
		"_Formatter":   reflect.ValueOf((*_fmt_Formatter)(nil)),
		"_GoStringer":  reflect.ValueOf((*_fmt_GoStringer)(nil)),
		"_ScanState":   reflect.ValueOf((*_fmt_ScanState)(nil)),
		"_Scanner":     reflect.ValueOf((*_fmt_Scanner)(nil)),
		"_State":       reflect.ValueOf((*_fmt_State)(nil)),
		"_Stringer":    reflect.ValueOf((*_fmt_Stringer)(nil)),
	}

	// errors
	Symbols["errors/errors"] = map[string]reflect.Value{
		"As":     reflect.ValueOf(errors.As),
		"Is":     reflect.ValueOf(errors.Is),
		"Join":   reflect.ValueOf(errors.Join),
		"New":    reflect.ValueOf(errors.New),
		"Unwrap": reflect.ValueOf(errors.Unwrap),
	}

	// strings
	Symbols["strings/strings"] = map[string]reflect.Value{
		"Clone":          reflect.ValueOf(strings.Clone),
		"Compare":        reflect.ValueOf(strings.Compare),
		"Contains":       reflect.ValueOf(strings.Contains),
		"ContainsAny":    reflect.ValueOf(strings.ContainsAny),
		"ContainsFunc":   reflect.ValueOf(strings.ContainsFunc),
		"ContainsRune":   reflect.ValueOf(strings.ContainsRune),
		"Count":          reflect.ValueOf(strings.Count),
		"Cut":            reflect.ValueOf(strings.Cut),
		"CutPrefix":      reflect.ValueOf(strings.CutPrefix),
		"CutSuffix":      reflect.ValueOf(strings.CutSuffix),
		"EqualFold":      reflect.ValueOf(strings.EqualFold),
		"Fields":         reflect.ValueOf(strings.Fields),
		"FieldsFunc":     reflect.ValueOf(strings.FieldsFunc),
		"HasPrefix":      reflect.ValueOf(strings.HasPrefix),
		"HasSuffix":      reflect.ValueOf(strings.HasSuffix),
		"Index":          reflect.ValueOf(strings.Index),
		"IndexAny":       reflect.ValueOf(strings.IndexAny),
		"IndexByte":      reflect.ValueOf(strings.IndexByte),
		"IndexFunc":      reflect.ValueOf(strings.IndexFunc),
		"IndexRune":      reflect.ValueOf(strings.IndexRune),
		"Join":           reflect.ValueOf(strings.Join),
		"LastIndex":      reflect.ValueOf(strings.LastIndex),
		"LastIndexAny":   reflect.ValueOf(strings.LastIndexAny),
		"LastIndexByte":  reflect.ValueOf(strings.LastIndexByte),
		"LastIndexFunc":  reflect.ValueOf(strings.LastIndexFunc),
		"Map":            reflect.ValueOf(strings.Map),
		"NewReader":      reflect.ValueOf(strings.NewReader),
		"NewReplacer":    reflect.ValueOf(strings.NewReplacer),
		"Repeat":         reflect.ValueOf(strings.Repeat),
		"Replace":        reflect.ValueOf(strings.Replace),
		"ReplaceAll":     reflect.ValueOf(strings.ReplaceAll),
		"Split":          reflect.ValueOf(strings.Split),
		"SplitAfter":     reflect.ValueOf(strings.SplitAfter),
		"SplitAfterN":    reflect.ValueOf(strings.SplitAfterN),
		"SplitN":         reflect.ValueOf(strings.SplitN),
		"ToLower":        reflect.ValueOf(strings.ToLower),
		"ToLowerSpecial": reflect.ValueOf(strings.ToLowerSpecial),
		"ToTitle":        reflect.ValueOf(strings.ToTitle),
		"ToTitleSpecial": reflect.ValueOf(strings.ToTitleSpecial),
		"ToUpper":        reflect.ValueOf(strings.ToUpper),
		"ToUpperSpecial": reflect.ValueOf(strings.ToUpperSpecial),
		"ToValidUTF8":    reflect.ValueOf(strings.ToValidUTF8),
		"Trim":           reflect.ValueOf(strings.Trim),
		"TrimFunc":       reflect.ValueOf(strings.TrimFunc),
		"TrimLeft":       reflect.ValueOf(strings.TrimLeft),
		"TrimLeftFunc":   reflect.ValueOf(strings.TrimLeftFunc),
		"TrimPrefix":     reflect.ValueOf(strings.TrimPrefix),
		"TrimRight":      reflect.ValueOf(strings.TrimRight),
		"TrimRightFunc":  reflect.ValueOf(strings.TrimRightFunc),
		"TrimSpace":      reflect.ValueOf(strings.TrimSpace),
		"TrimSuffix":     reflect.ValueOf(strings.TrimSuffix),
		"Builder":        reflect.ValueOf((*strings.Builder)(nil)),
		"Reader":         reflect.ValueOf((*strings.Reader)(nil)),
		"Replacer":       reflect.ValueOf((*strings.Replacer)(nil)),
	}

	// math
	Symbols["math/math"] = map[string]reflect.Value{
		"Abs":             reflect.ValueOf(math.Abs),
		"Acos":            reflect.ValueOf(math.Acos),
		"Asin":            reflect.ValueOf(math.Asin),
		"Atan":            reflect.ValueOf(math.Atan),
		"Atan2":           reflect.ValueOf(math.Atan2),
		"Ceil":            reflect.ValueOf(math.Ceil),
		"Cos":             reflect.ValueOf(math.Cos),
		"E":               reflect.ValueOf(constant.MakeFromLiteral("2.71828182845904523536028747135266249775724709369995957496696762566337824315673231520670375558666729784504486779277967997696994772644702281675346915668215131895555530285035761295375777990557253360748291015625", token.FLOAT, 0)),
		"Exp":             reflect.ValueOf(math.Exp),
		"Exp2":            reflect.ValueOf(math.Exp2),
		"Floor":           reflect.ValueOf(math.Floor),
		"Inf":             reflect.ValueOf(math.Inf),
		"IsInf":           reflect.ValueOf(math.IsInf),
		"IsNaN":           reflect.ValueOf(math.IsNaN),
		"Log":             reflect.ValueOf(math.Log),
		"Log10":           reflect.ValueOf(math.Log10),
		"Log2":            reflect.ValueOf(math.Log2),
		"Max":             reflect.ValueOf(math.Max),
		"MaxFloat64":      reflect.ValueOf(constant.MakeFromLiteral("179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368", token.FLOAT, 0)),
		"MaxInt":          reflect.ValueOf(constant.MakeFromLiteral("9223372036854775807", token.INT, 0)),
		"Min":             reflect.ValueOf(math.Min),
		"MinInt":          reflect.ValueOf(constant.MakeFromLiteral("-9223372036854775808", token.INT, 0)),
		"Mod":             reflect.ValueOf(math.Mod),
		"NaN":             reflect.ValueOf(math.NaN),
		"Pi":              reflect.ValueOf(constant.MakeFromLiteral("3.141592653589793238462643383279502884197169399375105820974944594789982923695635954704435713335896673485663389728754819466702315787113662862838515639906529162340867271374644786874341662041842937469482421875", token.FLOAT, 0)),
		"Pow":             reflect.ValueOf(math.Pow),
		"Pow10":           reflect.ValueOf(math.Pow10),
		"Round":           reflect.ValueOf(math.Round),
		"Sin":             reflect.ValueOf(math.Sin),
		"Sqrt":            reflect.ValueOf(math.Sqrt),
		"Sqrt2":           reflect.ValueOf(constant.MakeFromLiteral("1.414213562373095048801688724209698078569671875376948073176679739576083351575381440094441524123797447886801949755143139115339040409162552642832693297721230919563348109313505318596071447245776653289794921875", token.FLOAT, 0)),
		"Tan":             reflect.ValueOf(math.Tan),
		"Trunc":           reflect.ValueOf(math.Trunc),
	}

	// math/rand
	Symbols["math/rand/rand"] = map[string]reflect.Value{
		"Float32":    reflect.ValueOf(rand.Float32),
		"Float64":    reflect.ValueOf(rand.Float64),
		"Int":        reflect.ValueOf(rand.Int),
		"Intn":       reflect.ValueOf(rand.Intn),
		"New":        reflect.ValueOf(rand.New),
		"NewSource":  reflect.ValueOf(rand.NewSource),
		"Seed":       reflect.ValueOf(rand.Seed),
		"Rand":       reflect.ValueOf((*rand.Rand)(nil)),
		"Source":     reflect.ValueOf((*rand.Source)(nil)),
		"Source64":   reflect.ValueOf((*rand.Source64)(nil)),
		"Zipf":       reflect.ValueOf((*rand.Zipf)(nil)),
	}

	// sort
	Symbols["sort/sort"] = map[string]reflect.Value{
		"Float64s":          reflect.ValueOf(sort.Float64s),
		"Float64sAreSorted": reflect.ValueOf(sort.Float64sAreSorted),
		"Ints":              reflect.ValueOf(sort.Ints),
		"IntsAreSorted":     reflect.ValueOf(sort.IntsAreSorted),
		"IsSorted":          reflect.ValueOf(sort.IsSorted),
		"Search":            reflect.ValueOf(sort.Search),
		"SearchFloat64s":    reflect.ValueOf(sort.SearchFloat64s),
		"SearchInts":        reflect.ValueOf(sort.SearchInts),
		"SearchStrings":     reflect.ValueOf(sort.SearchStrings),
		"Slice":             reflect.ValueOf(sort.Slice),
		"SliceIsSorted":     reflect.ValueOf(sort.SliceIsSorted),
		"SliceStable":       reflect.ValueOf(sort.SliceStable),
		"Sort":              reflect.ValueOf(sort.Sort),
		"Stable":            reflect.ValueOf(sort.Stable),
		"Strings":           reflect.ValueOf(sort.Strings),
		"StringsAreSorted":  reflect.ValueOf(sort.StringsAreSorted),
		"Float64Slice":      reflect.ValueOf((*sort.Float64Slice)(nil)),
		"IntSlice":          reflect.ValueOf((*sort.IntSlice)(nil)),
		"Interface":         reflect.ValueOf((*sort.Interface)(nil)),
		"StringSlice":       reflect.ValueOf((*sort.StringSlice)(nil)),
	}

	// strconv
	Symbols["strconv/strconv"] = map[string]reflect.Value{
		"AppendBool":             reflect.ValueOf(strconv.AppendBool),
		"AppendFloat":            reflect.ValueOf(strconv.AppendFloat),
		"AppendInt":              reflect.ValueOf(strconv.AppendInt),
		"AppendQuote":            reflect.ValueOf(strconv.AppendQuote),
		"AppendUint":             reflect.ValueOf(strconv.AppendUint),
		"Atoi":                   reflect.ValueOf(strconv.Atoi),
		"CanBackquote":           reflect.ValueOf(strconv.CanBackquote),
		"FormatBool":             reflect.ValueOf(strconv.FormatBool),
		"FormatFloat":            reflect.ValueOf(strconv.FormatFloat),
		"FormatInt":              reflect.ValueOf(strconv.FormatInt),
		"FormatUint":             reflect.ValueOf(strconv.FormatUint),
		"IntSize":                reflect.ValueOf(strconv.IntSize),
		"IsGraphic":              reflect.ValueOf(strconv.IsGraphic),
		"IsPrint":                reflect.ValueOf(strconv.IsPrint),
		"Itoa":                   reflect.ValueOf(strconv.Itoa),
		"ParseBool":              reflect.ValueOf(strconv.ParseBool),
		"ParseFloat":             reflect.ValueOf(strconv.ParseFloat),
		"ParseInt":               reflect.ValueOf(strconv.ParseInt),
		"ParseUint":              reflect.ValueOf(strconv.ParseUint),
		"Quote":                  reflect.ValueOf(strconv.Quote),
		"QuoteRune":              reflect.ValueOf(strconv.QuoteRune),
		"QuoteRuneToASCII":       reflect.ValueOf(strconv.QuoteRuneToASCII),
		"QuoteRuneToGraphic":     reflect.ValueOf(strconv.QuoteRuneToGraphic),
		"QuoteToASCII":           reflect.ValueOf(strconv.QuoteToASCII),
		"QuoteToGraphic":         reflect.ValueOf(strconv.QuoteToGraphic),
		"Unquote":                reflect.ValueOf(strconv.Unquote),
		"UnquoteChar":            reflect.ValueOf(strconv.UnquoteChar),
		"ErrRange":               reflect.ValueOf(&strconv.ErrRange).Elem(),
		"ErrSyntax":              reflect.ValueOf(&strconv.ErrSyntax).Elem(),
		"NumError":               reflect.ValueOf((*strconv.NumError)(nil)),
	}
}

// Interface wrappers for fmt package (required by yaegi)
type _fmt_Formatter struct {
	IValue  interface{}
	WFormat func(f fmt.State, verb rune)
}

func (W _fmt_Formatter) Format(f fmt.State, verb rune) { W.WFormat(f, verb) }

type _fmt_GoStringer struct {
	IValue    interface{}
	WGoString func() string
}

func (W _fmt_GoStringer) GoString() string { return W.WGoString() }

type _fmt_ScanState struct {
	IValue      interface{}
	WRead       func(buf []byte) (n int, err error)
	WReadRune   func() (r rune, size int, err error)
	WSkipSpace  func()
	WToken      func(skipSpace bool, f func(rune) bool) (token []byte, err error)
	WUnreadRune func() error
	WWidth      func() (wid int, ok bool)
}

func (W _fmt_ScanState) Read(buf []byte) (n int, err error)        { return W.WRead(buf) }
func (W _fmt_ScanState) ReadRune() (r rune, size int, err error)   { return W.WReadRune() }
func (W _fmt_ScanState) SkipSpace()                                { W.WSkipSpace() }
func (W _fmt_ScanState) Token(s bool, f func(rune) bool) ([]byte, error) { return W.WToken(s, f) }
func (W _fmt_ScanState) UnreadRune() error                         { return W.WUnreadRune() }
func (W _fmt_ScanState) Width() (wid int, ok bool)                 { return W.WWidth() }

type _fmt_Scanner struct {
	IValue interface{}
	WScan  func(state fmt.ScanState, verb rune) error
}

func (W _fmt_Scanner) Scan(state fmt.ScanState, verb rune) error { return W.WScan(state, verb) }

type _fmt_State struct {
	IValue     interface{}
	WFlag      func(c int) bool
	WPrecision func() (prec int, ok bool)
	WWidth     func() (wid int, ok bool)
	WWrite     func(b []byte) (n int, err error)
}

func (W _fmt_State) Flag(c int) bool              { return W.WFlag(c) }
func (W _fmt_State) Precision() (prec int, ok bool) { return W.WPrecision() }
func (W _fmt_State) Width() (wid int, ok bool)     { return W.WWidth() }
func (W _fmt_State) Write(b []byte) (n int, err error) { return W.WWrite(b) }

type _fmt_Stringer struct {
	IValue  interface{}
	WString func() string
}

func (W _fmt_Stringer) String() string {
	if W.WString == nil {
		return ""
	}
	return W.WString()
}
