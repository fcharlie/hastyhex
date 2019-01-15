#include <algorithm>
#include <cctype>
#include <cerrno>
#include <cstdio>
#include <cstring>
#include <functional>
#include <string_view>
#include <system_error>
#include <vector>

namespace planck {
struct error_code {
  std::string message;
  int code{0};
  explicit operator bool() const noexcept { return code != 0; }
};
// final_act
// https://github.com/Microsoft/GSL/blob/ebe7ebfd855a95eb93783164ffb342dbd85cbc27/include/gsl/gsl_util#L85-L89
template <class F> class final_act {
public:
  explicit final_act(F f) noexcept : f_(std::move(f)), invoke_(true) {}

  final_act(final_act &&other) noexcept
      : f_(std::move(other.f_)), invoke_(other.invoke_) {
    other.invoke_ = false;
  }

  final_act(const final_act &) = delete;
  final_act &operator=(const final_act &) = delete;

  ~final_act() noexcept {
    if (invoke_)
      f_();
  }

private:
  F f_;
  bool invoke_;
};

// finally() - convenience function to generate a final_act
template <class F> inline final_act<F> finally(const F &f) noexcept {
  return final_act<F>(f);
}

template <class F> inline final_act<F> finally(F &&f) noexcept {
  return final_act<F>(std::forward<F>(f));
}

inline error_code make_error_code(int val, std::string_view msg) {
  return error_code{std::string(msg), val};
}
inline error_code make_error_code(std::string_view msg) {
  return error_code{std::string(msg), -1};
}

template <class _Tp>
using make_unsigned_t = typename std::make_unsigned<_Tp>::type;

struct from_chars_result {
  const char *ptr;
  std::errc ec;
};

inline unsigned char _Digit_from_char(const char _Ch) noexcept { // strengthened
  // convert ['0', '9'] ['A', 'Z'] ['a', 'z'] to [0, 35], everything else to 255
  static constexpr unsigned char _Digit_from_byte[] = {
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   255, 255,
      255, 255, 255, 255, 255, 10,  11,  12,  13,  14,  15,  16,  17,  18,  19,
      20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,
      35,  255, 255, 255, 255, 255, 255, 10,  11,  12,  13,  14,  15,  16,  17,
      18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  32,
      33,  34,  35,  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
      255};
  return _Digit_from_byte[static_cast<unsigned char>(_Ch)];
}

template <class _RawTy>
inline from_chars_result
_Integer_from_chars(const char *const _First, const char *const _Last,
                    _RawTy &_Raw_value,
                    const int _Base) noexcept { // strengthened

  bool _Minus_sign = false;

  const char *_Next = _First;

  if (std::is_signed<_RawTy>::value) {
    if (_Next != _Last && *_Next == '-') {
      _Minus_sign = true;
      ++_Next;
    }
  }

  using _Unsigned = make_unsigned_t<_RawTy>;

  constexpr _Unsigned _Uint_max = static_cast<_Unsigned>(-1);
  constexpr _Unsigned _Int_max = static_cast<_Unsigned>(_Uint_max >> 1);
  constexpr _Unsigned _Abs_int_min = static_cast<_Unsigned>(_Int_max + 1);

  _Unsigned _Risky_val;
  _Unsigned _Max_digit;

  if (std::is_signed<_RawTy>::value) {
    if (_Minus_sign) {
      _Risky_val = static_cast<_Unsigned>(_Abs_int_min / _Base);
      _Max_digit = static_cast<_Unsigned>(_Abs_int_min % _Base);
    } else {
      _Risky_val = static_cast<_Unsigned>(_Int_max / _Base);
      _Max_digit = static_cast<_Unsigned>(_Int_max % _Base);
    }
  } else {
    _Risky_val = static_cast<_Unsigned>(_Uint_max / _Base);
    _Max_digit = static_cast<_Unsigned>(_Uint_max % _Base);
  }

  _Unsigned _Value = 0;

  bool _Overflowed = false;

  for (; _Next != _Last; ++_Next) {
    const unsigned char _Digit = _Digit_from_char(*_Next);

    if (_Digit >= _Base) {
      break;
    }

    if (_Value < _Risky_val // never overflows
        || (_Value == _Risky_val &&
            _Digit <= _Max_digit)) { // overflows for certain digits
      _Value = static_cast<_Unsigned>(_Value * _Base + _Digit);
    } else {              // _Value > _Risky_val always overflows
      _Overflowed = true; // keep going, _Next still needs to be updated, _Value
                          // is now irrelevant
    }
  }

  if (_Next - _First == static_cast<ptrdiff_t>(_Minus_sign)) {
    return {_First, std::errc::invalid_argument};
  }

  if (_Overflowed) {
    return {_Next, std::errc::result_out_of_range};
  }

  if (std::is_signed<_RawTy>::value) {
    if (_Minus_sign) {
      _Value = static_cast<_Unsigned>(0 - _Value);
    }
  }

  _Raw_value =
      static_cast<_RawTy>(_Value); // implementation-defined for negative,
                                   // N4713 7.8 [conv.integral]/3

  return {_Next, std::errc{}};
}

inline from_chars_result
from_chars(const char *const _First, const char *const _Last, char &_Value,
           const int _Base = 10) noexcept { // strengthened
  return _Integer_from_chars(_First, _Last, _Value, _Base);
}
inline from_chars_result
from_chars(const char *const _First, const char *const _Last,
           signed char &_Value,
           const int _Base = 10) noexcept { // strengthened
  return _Integer_from_chars(_First, _Last, _Value, _Base);
}
inline from_chars_result
from_chars(const char *const _First, const char *const _Last,
           unsigned char &_Value,
           const int _Base = 10) noexcept { // strengthened
  return _Integer_from_chars(_First, _Last, _Value, _Base);
}
inline from_chars_result
from_chars(const char *const _First, const char *const _Last, short &_Value,
           const int _Base = 10) noexcept { // strengthened
  return _Integer_from_chars(_First, _Last, _Value, _Base);
}
inline from_chars_result
from_chars(const char *const _First, const char *const _Last,
           unsigned short &_Value,
           const int _Base = 10) noexcept { // strengthened
  return _Integer_from_chars(_First, _Last, _Value, _Base);
}
inline from_chars_result
from_chars(const char *const _First, const char *const _Last, int &_Value,
           const int _Base = 10) noexcept { // strengthened
  return _Integer_from_chars(_First, _Last, _Value, _Base);
}
inline from_chars_result
from_chars(const char *const _First, const char *const _Last,
           unsigned int &_Value,
           const int _Base = 10) noexcept { // strengthened
  return _Integer_from_chars(_First, _Last, _Value, _Base);
}
inline from_chars_result
from_chars(const char *const _First, const char *const _Last, long &_Value,
           const int _Base = 10) noexcept { // strengthened
  return _Integer_from_chars(_First, _Last, _Value, _Base);
}
inline from_chars_result
from_chars(const char *const _First, const char *const _Last,
           unsigned long &_Value,
           const int _Base = 10) noexcept { // strengthened
  return _Integer_from_chars(_First, _Last, _Value, _Base);
}
inline from_chars_result
from_chars(const char *const _First, const char *const _Last, long long &_Value,
           const int _Base = 10) noexcept { // strengthened
  return _Integer_from_chars(_First, _Last, _Value, _Base);
}
inline from_chars_result
from_chars(const char *const _First, const char *const _Last,
           unsigned long long &_Value,
           const int _Base = 10) noexcept { // strengthened
  return _Integer_from_chars(_First, _Last, _Value, _Base);
}

template <typename Integer>
inline from_chars_result from_chars(std::string_view sv, Integer &i,
                                    const int base = 10) noexcept {
  return from_chars(sv.data(), sv.data() + sv.size(), i, base);
}

class ParseArgv {
public:
  enum HasArgs {
    required_argument, /// -i 11 or -i=xx
    no_argument,
    optional_argument /// -s --long --long=xx
  };
  struct option {
    const char *name;
    HasArgs has_args;
    int val;
  };
  using ArgumentCallback =
      std::function<bool(int, const char *optarg, const char *raw)>;
  ParseArgv(int argc, char *const *argv) : argc_(argc), argv_(argv) {}
  ParseArgv(const ParseArgv &) = delete;
  ParseArgv &operator=(const ParseArgv &) = delete;

  error_code ParseArgument(const std::vector<option> &opts,
                           const ArgumentCallback &callback) {
    if (argc_ == 0 || argv_ == nullptr) {
      return make_error_code("invalid argument input");
    };
    index = 1;
    for (; index < argc_; index++) {
      std::string_view arg = argv_[index];
      if (arg[0] != '-') {
        uargs.push_back(arg);
        continue;
      }
      auto ec = ParseInternal(arg, opts, callback);
      if (ec) {
        return ec;
      }
    }
    return error_code();
  }
  const std::vector<std::string_view> &UnresolvedArgs() const { return uargs; }

private:
  int argc_;
  char *const *argv_;
  std::vector<std::string_view> uargs;
  int index{0};
  error_code ParseInternal(std::string_view arg,
                           const std::vector<option> &opts,
                           const ArgumentCallback &callback) {
    /*
    -x ; -x value -Xvalue
    --xy;--xy=value;--xy value
    */
    if (arg.size() < 2) {
      return make_error_code("Invalid argument");
    }
    int ch = -1;
    HasArgs ha = optional_argument;
    const char *optarg = nullptr;

    if (arg[1] == '-') {
      /// parse long
      /// --name value; --name=value
      std::string_view name;
      auto pos = arg.find('=');
      if (pos != std::wstring_view::npos) {
        if (pos + 1 >= arg.size()) {
          return make_error_code(
              std::string("Incorrect argument: ").append(arg));
        }
        name = arg.substr(2, pos - 2);
        optarg = arg.data() + pos + 1;
      } else {
        name = arg.substr(2);
      }
      for (auto &o : opts) {
        if (name.compare(o.name) == 0) {
          ch = o.val;
          ha = o.has_args;
          break;
        }
      }
    } else {
      /// parse short
      ch = arg[1];

      /// -x=xxx
      if (arg.size() == 3 && arg[2] == '=') {
        return make_error_code(std::string("Incorrect argument: ").append(arg));
      }
      if (arg.size() > 3) {
        if (arg[2] == '=') {
          optarg = arg.data() + 3;
        } else {
          optarg = arg.data() + 2;
        }
      }
      for (auto &o : opts) {
        if (o.val == ch) {
          ha = o.has_args;
          break;
        }
      }
    }

    if (optarg != nullptr && ha == no_argument) {
      return make_error_code(std::string("Unacceptable input: ").append(arg));
    }
    if (optarg == nullptr && ha == required_argument) {
      if (index + 1 >= argc_) {
        return make_error_code(
            std::string("Option name cannot be empty: ").append(arg));
      }
      optarg = argv_[index + 1];
      index++;
    }
    if (callback(ch, optarg, arg.data())) {
      return error_code();
    }
    return make_error_code("skipped");
  }
};

} // namespace planck

#define PROGRAM_NAME L"hastyhex"

static const char hex[] = "0123456789abcdef";

static int color(int b) {
#define CN 0x37 /* null    */
#define CS 0x92 /* space   */
#define CP 0x96 /* print   */
#define CC 0x95 /* control */
#define CH 0x93 /* high    */
  static const unsigned char table[] = {
      CN, CC, CC, CC, CC, CC, CC, CC, CC, CC, CS, CS, CS, CS, CC, CC, CC, CC,
      CC, CC, CC, CC, CC, CC, CC, CC, CC, CC, CC, CC, CC, CC, CS, CP, CP, CP,
      CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP,
      CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP,
      CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP,
      CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP,
      CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP, CP,
      CP, CC, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH,
      CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH,
      CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH,
      CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH,
      CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH,
      CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH,
      CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH, CH,
      CH, CH, CH, CH};
  return table[b];
}

static int display(int b) {
  static const char table[] = {
      0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e,
      0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e,
      0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x20, 0x21, 0x22, 0x23,
      0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
      0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b,
      0x3c, 0x3d, 0x3e, 0x3f, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
      0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52, 0x53,
      0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
      0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6a, 0x6b,
      0x6c, 0x6d, 0x6e, 0x6f, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
      0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e,
      0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e,
      0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e,
      0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e,
      0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e,
      0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e,
      0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e,
      0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e,
      0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e,
      0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e,
      0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e,
      0x2e, 0x2e, 0x2e, 0x2e,
  };
  return table[b];
}

static void process_color(FILE *in, FILE *out, int64_t len) {
  size_t i, n;
  unsigned long offset = 0;
  unsigned char input[16] = {0};
  constexpr const uint64_t inputlen = sizeof(input);
  char colortemplate[] =
      "00000000  "
      "\33[XXm## \33[XXm## \33[XXm## \33[XXm## "
      "\33[XXm## \33[XXm## \33[XXm## \33[XXm##  "
      "\33[XXm## \33[XXm## \33[XXm## \33[XXm## "
      "\33[XXm## \33[XXm## \33[XXm## \33[XXm##  "
      "\33[XXm.\33[XXm.\33[XXm.\33[XXm.\33[XXm.\33[XXm.\33[XXm.\33[XXm."
      "\33[XXm.\33[XXm.\33[XXm.\33[XXm.\33[XXm.\33[XXm.\33[XXm.\33[XXm."
      "\33[0m\n";
  static const int slots[] = {
      /* ANSI-color, hex, ANSI-color, ASCII */
      12,  15,  142, 145, 20,  23,  148, 151, 28,  31,  154, 157, 36,
      39,  160, 163, 44,  47,  166, 169, 52,  55,  172, 175, 60,  63,
      178, 181, 68,  71,  184, 187, 77,  80,  190, 193, 85,  88,  196,
      199, 93,  96,  202, 205, 101, 104, 208, 211, 109, 112, 214, 217,
      117, 120, 220, 223, 125, 128, 226, 229, 133, 136, 232, 235};
  uint64_t maxlen = len > 0 ? len : UINT64_MAX;

  do {
    auto rn = (std::min)(maxlen, inputlen);
    n = fread(input, 1, (int)rn, in);
    maxlen -= n;
    /* Write the offset */
    for (i = 0; i < 8; i++)
      colortemplate[i] = hex[(offset >> (28 - i * 4)) & 15];

    /* Fill out the colortemplate */
    for (i = 0; i < 16; i++) {
      /* Use a fixed loop count instead of "n" to encourage loop
       * unrolling by the compiler. Empty bytes will be erased
       * later.
       */
      int v = input[i];
      int c = color(v);
      colortemplate[slots[i * 4 + 0] + 0] = hex[c >> 4];
      colortemplate[slots[i * 4 + 0] + 1] = hex[c & 15];
      colortemplate[slots[i * 4 + 1] + 0] = hex[v >> 4];
      colortemplate[slots[i * 4 + 1] + 1] = hex[v & 15];
      colortemplate[slots[i * 4 + 2] + 0] = hex[c >> 4];
      colortemplate[slots[i * 4 + 2] + 1] = hex[c & 15];
      colortemplate[slots[i * 4 + 3] + 0] = display(v);
    }

    /* Erase any trailing bytes */
    for (i = n; i < 16; i++) {
      /* This loop is only used once: the last line of output. The
       * branch predictor will quickly learn that it's never taken.
       */
      colortemplate[slots[i * 4 + 0] + 0] = '0';
      colortemplate[slots[i * 4 + 0] + 1] = '0';
      colortemplate[slots[i * 4 + 1] + 0] = ' ';
      colortemplate[slots[i * 4 + 1] + 1] = ' ';
      colortemplate[slots[i * 4 + 2] + 0] = '0';
      colortemplate[slots[i * 4 + 2] + 1] = '0';
      colortemplate[slots[i * 4 + 3] + 0] = ' ';
    }

    if (!fwrite(colortemplate, sizeof(colortemplate) - 1, 1, out))
      break; /* Output error */
    offset += 16;
  } while (n == 16 && maxlen > 0);
}

static void process_plain(FILE *in, FILE *out, int64_t len) {
  size_t i, n;
  unsigned long offset = 0;
  unsigned char input[16] = {0};
  constexpr const uint64_t inputlen = sizeof(input);
  char colortemplate[] =
      "00000000  ## ## ## ## ## ## ## ##  ## ## ## ## ## ## ## ##  "
      "................\n";
  static const int slots[] = {10, 60, 13, 61, 16, 62, 19, 63, 22, 64, 25,
                              65, 28, 66, 31, 67, 35, 68, 38, 69, 41, 70,
                              44, 71, 47, 72, 50, 73, 53, 74, 56, 75};

  uint64_t maxlen = len > 0 ? len : UINT64_MAX;
  do {
    auto rn = (std::min)(maxlen, inputlen);
    n = fread(input, 1, (int)rn, in);
    maxlen -= n;

    /* Write the offset */
    for (i = 0; i < 8; i++)
      colortemplate[i] = hex[(offset >> (28 - i * 4)) & 15];

    /* Fill out the template */
    for (i = 0; i < 16; i++) {
      int v = input[i];
      colortemplate[slots[i * 2 + 0] + 0] = hex[v >> 4];
      colortemplate[slots[i * 2 + 0] + 1] = hex[v & 15];
      colortemplate[slots[i * 2 + 1] + 0] = display(v);
    }

    /* Erase any trailing bytes */
    for (i = n; i < 16; i++) {
      colortemplate[slots[i * 2 + 0] + 0] = ' ';
      colortemplate[slots[i * 2 + 0] + 1] = ' ';
      colortemplate[slots[i * 2 + 1] + 0] = ' ';
    }

    if (!fwrite(colortemplate, sizeof(colortemplate) - 1, 1, out))
      break;
    offset += 16;
  } while (n == 16 && maxlen > 0);
}

struct BinaryOptions {
  enum BufferMode { Auto, Line, Full };
  BufferMode bm{Auto};
  std::string file;
  std::string out;
  int64_t length{-1};
  uint64_t seek{0};
  bool plain{false};
};

void PrintUsage() {
  const char *ua = R"(OVERVIEW: hastyhex
Usage: hastyhex [options] <input>
OPTIONS:
  -h [--help]                      Print hastyhex usage information and exit
  -n [--length]                    Read only N bytes from the input.
  -s [--seek]                      Read from the specified offset
  -o [--out]                       Output to file instead of standard output
  -p [--plain]                     Do not output color ("plain")
  -l [--line]                      Force output line-buffered
  -f [--force]                     Force output fully-buffered

Example:
  hastyhex example.bin

)";
  printf("%s", ua);
}

bool ParseArgv(int argc, char **argv, BinaryOptions &bo) {
  std::vector<planck::ParseArgv::option> opts = {
      {"help", planck::ParseArgv::no_argument, 'h'},
      {"length", planck::ParseArgv::required_argument, 'n'},
      {"seek", planck::ParseArgv::required_argument, 's'},
      {"plain", planck::ParseArgv::no_argument, 'p'},
      {"out", planck::ParseArgv::required_argument, 'o'},
      {"force", planck::ParseArgv::no_argument, 'f'},
      {"line", planck::ParseArgv::no_argument, 'l'}
      //
  };
  planck::ParseArgv pv(argc, argv);
  auto ec = pv.ParseArgument(opts, [&](int ch, const char *optarg,
                                       const char *raw) {
    switch (ch) {
    case 'h':
      PrintUsage();
      exit(0);
      break;
    case 'o':
      bo.out = optarg;
      break;
    case 'n': {
      int64_t n;
      auto ec = planck::from_chars(optarg, n, 10);
      if (ec.ec == std::errc{}) {
        bo.length = n;
      }
    } break;
    case 's': {
      int64_t n;
      auto ec = planck::from_chars(optarg, n, 10);
      if (ec.ec == std::errc{}) {
        bo.seek = n;
      }
    } break;
    case 'p':
      bo.plain = true;
      break;
    case 'l':
      bo.bm = BinaryOptions::Line;
      break;
    case 'f':
      bo.bm = BinaryOptions::Full;
      break;
    default:
      fprintf(stderr, "Error Argument: %s\n", raw != nullptr ? raw : "unknown");
      return false;
    }
    return true;
  });

  if (ec) {
    fprintf(stderr, "ParseArgv: %s\n", ec.message.c_str());
    return false;
  }
  if (pv.UnresolvedArgs().empty()) {
    fprintf(stderr, "Too few arguments\n");
    return false;
  }
  bo.file = pv.UnresolvedArgs()[0];
  return true;
}

int main(int argc, char *argv[]) {
  FILE *in = stdin;
  FILE *out = stdout;
  BinaryOptions bo;
  if (!ParseArgv(argc, argv, bo)) {
    return 1;
  }
  if ((in = fopen(bo.file.c_str(), "rb")) == nullptr) {
    fprintf(stderr, "hastyhex: open '%s': %s\n", bo.file.data(),
            strerror(errno));
    return 1;
  }
  auto c1 = planck::finally([&] { fclose(in); });
  if (!bo.out.empty()) {
    if ((out = fopen(bo.out.c_str(), "wb")) == nullptr) {
      fprintf(stderr, "hastyhex: open '%s': %s\n", bo.out.c_str(),
              strerror(errno));
      return 1;
    }
  }
  auto c2 = planck::finally([&] {
    if (out != stdout) {
      fclose(out);
    }
  });
  if (in != stdin) {
    if (fseeko64(in, bo.seek, SEEK_SET) != 0) {
      fprintf(stderr, "hastyhex: seek to %ld %s\n", bo.seek, strerror(errno));
      return 1;
    }
  }

  switch (bo.bm) {
    static char buf[1L << 18];
  case BinaryOptions::Auto:
    break;
  case BinaryOptions::Line:
    setvbuf(out, buf, _IOLBF, sizeof(buf));
    break;
  case BinaryOptions::Full:
    setvbuf(out, buf, _IOFBF, sizeof(buf));
    break;
  }

  if (bo.plain) {
    process_plain(in, out, bo.length);
  } else {
    process_color(in, out, bo.length);
  }

  return 0;
}
