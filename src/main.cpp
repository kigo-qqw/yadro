#include <algorithm>
#include <chrono>
#include <cstdint>
#include <cstdlib>
#include <ctime>
#include <deque>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <iostream>
#include <iterator>
#include <locale>
#include <memory>
#include <optional>
#include <ostream>
#include <regex>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

using i8 = std::int8_t;
using i16 = std::int16_t;
using i32 = std::int32_t;
using i64 = std::int64_t;

using u8 = std::uint8_t;
using u16 = std::uint16_t;
using u32 = std::uint32_t;
using u64 = std::uint64_t;

using c8 = char;

using f32 = float;
using f64 = double;

struct CliArguments {
  std::filesystem::path Path;

  static CliArguments ParseArguments(i32 argc, c8 **argv);
};

CliArguments CliArguments::ParseArguments([[maybe_unused]] i32 argc, c8 **argv) {
  return {.Path = argv[1]};  // no validation, missing from task
}

using EventId = i8;
using TableId = i64;
constexpr TableId kTableIdUnknown = -1;

// C++23 feature // https://en.cppreference.com/w/cpp/utility/to_underlying
template <class E> constexpr auto ToUnderlying(E e) noexcept -> std::underlying_type_t<E> {
  return static_cast<std::underlying_type_t<E>>(e);
}

enum class InputEventType : EventId {
  Came = 1,
  SitDown = 2,
  Wait = 3,
  Left = 4,
};

enum class OutputEventType : EventId {
  Left = 11,
  SitDown = 12,
  Error = 13,
};

enum class OutputErrorType : i8 {
  YouShallNotPass,
  NotOpenYet,
  PlaceIsBusy,
  ClientUnknown,
  ICanWaitNoLonger,
};

std::optional<std::string_view> OutputErrorTypeToString(OutputErrorType T) noexcept {
  switch (T) {
    using enum OutputErrorType;
    case YouShallNotPass:
      return {"YouShallNotPass"};
    case NotOpenYet:
      return {"NotOpenYet"};
    case PlaceIsBusy:
      return {"PlaceIsBusy"};
    case ClientUnknown:
      return {"ClientUnknown"};
    case ICanWaitNoLonger:
      return {"ICanWaitNoLonger!"};
    default:
      return std::nullopt;
  }
}

using TimeUnderlyingType = std::chrono::minutes;
struct Time : public TimeUnderlyingType {};

template <> struct std::formatter<Time> : std::formatter<std::string_view> {
  constexpr auto format(const Time &T, auto &ctx) const {
    std::chrono::hh_mm_ss<TimeUnderlyingType> HMS{T};
    auto &&S = std::format("{:02}:{:02}", HMS.hours().count(), HMS.minutes().count());
    return std::formatter<std::string_view>::format(S, ctx);
  }
};

struct Config {
  i64 TableCount;
  Time Start;
  Time End;
  i64 HourCost;
};

template <> struct std::formatter<Config> : std::formatter<std::string_view> {
  constexpr auto format(const Config &C, auto &ctx) const {
    auto &&S = std::format("Config{{.TableCount={}, .Start={}, .End={}, .HourCost={}}}", C.TableCount, C.Start, C.End,
                           C.HourCost);
    return std::formatter<std::string_view>::format(S, ctx);
  }
};

std::optional<Time> ParseTime(auto &IF) {
  if (IF.fail()) std::cout << "ParseTime IF.fail()" << std::endl;
  IF >> std::ws;

  Time T{};
  IF >> std::chrono::parse("%H:%M", T);

  if (IF.fail()) return std::nullopt;
  return {T};
}

std::optional<Config> ParseConfig(auto &IF) {
  decltype(std::declval<Config>().TableCount) TableCount{-1};
  IF >> TableCount;

  auto &&Start = ParseTime(IF);
  auto &&End = ParseTime(IF);

  decltype(std::declval<Config>().TableCount) HourCost{-1};
  IF >> HourCost;

  if (TableCount > 0 and Start and End and HourCost > 0)
    return {Config{
            .TableCount = TableCount,
            .Start = *Start,
            .End = *End,
            .HourCost = HourCost,
    }};
  return std::nullopt;
}

using ClientName = std::string;
using ClientNameRef = std::string_view;

class Event;
class OutputEvent;

u64 TableIndex(TableId Id) noexcept {
  return static_cast<u64>(Id - 1);
}

TableId TableIndexToId(u64 Index) noexcept {
  return static_cast<TableId>(Index + 1);
}

struct ClientState {
  TableId TableId;
};

struct ClientAtTableState {
  ClientNameRef ClientName;
  Time Start;
  Time End;
};

struct Table {
  std::optional<ClientAtTableState> State;
  decltype(std::declval<Config>().HourCost) Earned;
  Time TotalUsed;
};

class State final {
  public:
  State(Config C) : mTables(static_cast<u64>(C.TableCount)), mConfig{std::move(C)} {}

  struct StringHash {
    using is_transparent = void;
    [[nodiscard]] std::size_t operator()(const char *Str) const {
      return std::hash<std::string_view>{}(Str);
    }
    [[nodiscard]] std::size_t operator()(std::string_view Str) const {
      return std::hash<std::string_view>{}(Str);
    }
    [[nodiscard]] std::size_t operator()(const std::string &Str) const {
      return std::hash<std::string>{}(Str);
    }
  };

  using DataType = std::unordered_map<ClientName, ClientState, StringHash, std::equal_to<>>;

  DataType &Data() noexcept {
    return mData;
  }

  std::deque<ClientName> &WaitingQueue() noexcept {
    return mWaitingQueue;
  }

  std::vector<Table> &Tables() noexcept {
    return mTables;
  }

  const Config &Config() const noexcept {
    return mConfig;
  }

  void ProcessOutputEvent(const OutputEvent &OE);

  void ProcessCame(ClientNameRef Name) {
    auto &&[InsertedIt, IsInserted] = mData.emplace(Name, ClientState{.TableId = kTableIdUnknown});
    if (IsInserted) {
      mWaitingQueue.emplace_back(InsertedIt->first);
    }
  }

  void ProcessSitDown(ClientNameRef Name, TableId Id, Time Time) {
    auto &&It = mData.find(Name);
    if (It == std::end(mData)) return;

    if (It->second.TableId != kTableIdUnknown) {
      mTables[TableIndex(It->second.TableId)].State = std::nullopt;
    } else {
      auto &&[First, Last] = std::ranges::remove(mWaitingQueue, Name);
      mWaitingQueue.erase(First, Last);
    }

    mTables[TableIndex(Id)].State.emplace(
            ClientAtTableState{.ClientName = {It->first}, .Start = Time, .End = {Time::zero()}});
    It->second.TableId = Id;
  }

  void ProcessLeft(ClientNameRef Name, Time Time);

  friend std::ostream &operator<<(std::ostream &OS, State &S) {
    OS << "State{{mData=";
    for (auto &&[K, V] : S.Data()) {
      OS << "[" << K << "] = {" << V.TableId << "}, ";
    }
    OS << ", mWaitingQueue=";
    for (auto &&N : S.WaitingQueue()) {
      OS << N << ", ";
    }
    OS << ", mTables=";
    for (auto &&T : S.Tables()) {
      OS << "{";
      if (T.State) OS << "{" << T.State->ClientName << ", " << T.State->Start << ", " << T.State->End << "}";
      else OS << "std::nullopt";
      OS << ", " << T.Earned << ", " << std::format("{}", T.TotalUsed) << "}, ";
    }
    return OS << std::format(", mConfig={}}}", S.Config());
  }

  private:
  DataType mData;
  std::deque<ClientName> mWaitingQueue;
  std::vector<Table> mTables;  // TODO: change to smth-like boost::static_vector
  struct Config mConfig;
};


class Event {
  protected:
  friend std::formatter<Event>;
  Time mTime;
  EventId mId;

  public:
  Event(Time EventTime, EventId Id) : mTime{EventTime}, mId{Id} {}
  virtual ~Event() noexcept = default;
  virtual void Print(std::ostream &OS) const = 0;

  friend std::ostream &operator<<(std::ostream &OS, const Event &E) {
    OS << std::format("{}", E.mTime) << " " << static_cast<i64>(E.mId) << " ";
    E.Print(OS);
    return OS;
  }
};

class InputEvent : public Event {
  public:
  InputEvent(Time EventTime, InputEventType Type) : Event(EventTime, ToUnderlying(Type)) {}
  ~InputEvent() noexcept override = default;

  virtual void Process(State &S) const = 0;

  auto Type() const noexcept {
    return static_cast<InputEventType>(mId);
  }
};

class OutputEvent : public Event {
  public:
  OutputEvent(Time EventTime, OutputEventType Type) : Event(EventTime, ToUnderlying(Type)) {}
  ~OutputEvent() noexcept override = default;

  virtual void Process(State &S) const = 0;

  auto Type() const noexcept {
    return static_cast<OutputEventType>(mId);
  }
};

class LeftOutputEvent : public OutputEvent {
  ClientName mClientName;

  public:
  LeftOutputEvent(Time EventTime, ClientNameRef ClientName)
      : OutputEvent(EventTime, OutputEventType::Left), mClientName{ClientName} {}
  ~LeftOutputEvent() noexcept override = default;

  void Process(State &S) const override {
    S.ProcessLeft(mClientName, mTime);
  }

  void Print(std::ostream &OS) const override {
    OS << ClientName();
#ifndef NDEBUG
    OS << " (LeftOutputEvent)";
#endif
  }

  ClientNameRef ClientName() const noexcept {
    return mClientName;
  }
};

class SitDownOutputEvent : public OutputEvent {
  ClientName mClientName;
  TableId mTableId;

  public:
  SitDownOutputEvent(Time EventTime, ClientNameRef ClientName, TableId Id)
      : OutputEvent(EventTime, OutputEventType::SitDown), mClientName{ClientName}, mTableId{Id} {}
  ~SitDownOutputEvent() noexcept override = default;

  void Process(State &S) const override {
    S.ProcessSitDown(mClientName, mTableId, mTime);
  }

  void Print(std::ostream &OS) const override {
    OS << ClientName() << " " << TableId();
#ifndef NDEBUG
    OS << " (SitDownOutputEvent)";
#endif
  }

  ClientNameRef ClientName() const noexcept {
    return mClientName;
  }
  TableId TableId() const noexcept {
    return mTableId;
  }
};

class ErrorOutputEvent : public OutputEvent {
  OutputErrorType mErrorType;

  public:
  ErrorOutputEvent(Time EventTime, OutputErrorType ErrorType)
      : OutputEvent(EventTime, OutputEventType::Error), mErrorType{ErrorType} {}
  ~ErrorOutputEvent() noexcept override = default;

  void Process(State &S) const override {}

  void Print(std::ostream &OS) const override {
    auto &&S = OutputErrorTypeToString(ErrorType());
    if (S) OS << *S;
    else std::abort();  // internal error
#ifndef NDEBUG
    OS << " (ErrorOutputEvent)";
#endif
  }

  OutputErrorType ErrorType() const noexcept {
    return mErrorType;
  }
};

class CameInputEvent : public InputEvent {
  ClientName mClientName;

  public:
  CameInputEvent(Time EventTime, ClientNameRef ClientName)
      : InputEvent(EventTime, InputEventType::Came), mClientName{ClientName} {}
  ~CameInputEvent() noexcept override = default;

  void Process(State &S) const override {
    auto &D = S.Data();
    auto &&It = D.find(mClientName);

    if (mTime < S.Config().Start or mTime > S.Config().End) {
      S.ProcessOutputEvent(ErrorOutputEvent(mTime, OutputErrorType::NotOpenYet));
      return;
    }

    if (It != std::end(D)) {
      S.ProcessOutputEvent(ErrorOutputEvent(mTime, OutputErrorType::YouShallNotPass));
      return;
    }

    S.ProcessCame(mClientName);
  }

  void Print(std::ostream &OS) const override {
    OS << ClientName();
#ifndef NDEBUG
    OS << " (CameInputEvent)";
#endif
  }

  ClientNameRef ClientName() const noexcept {
    return mClientName;
  }
};

class SitDownInputEvent : public InputEvent {
  ClientName mClientName;
  TableId mTableId;

  public:
  SitDownInputEvent(Time EventTime, ClientNameRef ClientName, TableId Id)
      : InputEvent(EventTime, InputEventType::SitDown), mClientName{ClientName}, mTableId{Id} {}
  ~SitDownInputEvent() noexcept override = default;

  void Process(State &S) const override {
    auto &D = S.Data();
    auto &T = S.Tables();
    auto &&It = D.find(mClientName);

    if (It == std::end(D)) {
      S.ProcessOutputEvent(ErrorOutputEvent(mTime, OutputErrorType::ClientUnknown));
      return;
    }

    auto &TargetTable = T[TableIndex(mTableId)];

    if (TargetTable.State) {
      S.ProcessOutputEvent(ErrorOutputEvent(mTime, OutputErrorType::PlaceIsBusy));
      return;
    }
    S.ProcessSitDown(mClientName, mTableId, mTime);
  }

  void Print(std::ostream &OS) const override {
    OS << ClientName() << " " << TableId();
#ifndef NDEBUG
    OS << " (SitDownInputEvent)";
#endif
  }

  ClientNameRef ClientName() const noexcept {
    return mClientName;
  }

  TableId TableId() const noexcept {
    return mTableId;
  }
};

class WaitInputEvent : public InputEvent {
  ClientName mClientName;

  public:
  WaitInputEvent(Time EventTime, ClientNameRef ClientName)
      : InputEvent(EventTime, InputEventType::Wait), mClientName{ClientName} {}
  ~WaitInputEvent() noexcept override = default;

  void Process(State &S) const override {
    auto &D = S.Data();
    auto &T = S.Tables();
    auto &C = S.Config();

    auto &&AmountOfBusyTables = std::ranges::count_if(T, [](auto &&Tbl) { return Tbl.State.has_value(); });

    if (AmountOfBusyTables < C.TableCount) {
      S.ProcessOutputEvent(ErrorOutputEvent(mTime, OutputErrorType::ICanWaitNoLonger));
      return;
    }

    auto &&InWaitingQueue = static_cast<i64>(D.size()) - AmountOfBusyTables;

    if (InWaitingQueue > C.TableCount) {
      S.ProcessOutputEvent(LeftOutputEvent(mTime, mClientName));
      return;
    }
  }

  void Print(std::ostream &OS) const override {
    OS << ClientName();
#ifndef NDEBUG
    OS << " (WaitInputEvent)";
#endif
  }

  ClientNameRef ClientName() const noexcept {
    return mClientName;
  }
};

class LeftInputEvent : public InputEvent {
  ClientName mClientName;

  public:
  LeftInputEvent(Time EventTime, ClientNameRef ClientName)
      : InputEvent(EventTime, InputEventType::Left), mClientName{ClientName} {}
  ~LeftInputEvent() noexcept override = default;

  void Process(State &S) const override {
    auto &D = S.Data();
    auto &&It = D.find(mClientName);
    if (It == std::end(D)) {
      S.ProcessOutputEvent(ErrorOutputEvent(mTime, OutputErrorType::ClientUnknown));
      return;
    }
    S.ProcessLeft(mClientName, mTime);
  }

  void Print(std::ostream &OS) const override {
    OS << ClientName();
#ifndef NDEBUG
    OS << " (LeftInputEvent)";
#endif
  }

  ClientNameRef ClientName() const noexcept {
    return mClientName;
  }
};

std::optional<std::unique_ptr<InputEvent>> ParseInputEvent(auto &IF) {
  auto &&EventTime = ParseTime(IF);
  if (!EventTime) return std::nullopt;

  i64 RawId;
  IF >> RawId;
  auto &&Id = static_cast<InputEventType>(RawId);

  ClientName ClientName;
  IF >> ClientName;

  switch (Id) {
    using enum InputEventType;
    case Came:
      return {std::make_unique<CameInputEvent>(*EventTime, ClientName)};
    case SitDown: {
      TableId TId;
      IF >> TId;
      return {std::make_unique<SitDownInputEvent>(*EventTime, ClientName, TId)};
    }
    case Wait:
      return {std::make_unique<WaitInputEvent>(*EventTime, ClientName)};
    case Left:
      return {std::make_unique<LeftInputEvent>(*EventTime, ClientName)};
    default:
      return std::nullopt;
  }
}

void State::ProcessOutputEvent(const OutputEvent &OE) {
  // TODO: change to sink from some cfg
  std::cout << OE << std::endl;
  OE.Process(*this);
}

void State::ProcessLeft(ClientNameRef Name, Time Time) {
  auto &&It = mData.find(Name);
  if (It->second.TableId == kTableIdUnknown) {
    mData.erase(It);
    return;
  }
  auto &&I = TableIndex(It->second.TableId);

  if (auto &CS = mTables[I].State) {
    CS->End = Time;
    auto &&TimeDelta = CS->End - CS->Start;
    auto &&HoursUsed = std::chrono::ceil<std::chrono::hours>(TimeDelta);
    mTables[I].Earned += HoursUsed.count() * mConfig.HourCost;
    mTables[I].TotalUsed += TimeDelta;
  }

  mData.erase(It);

  if (not mWaitingQueue.empty()) {
    auto &&ClientToSit = mWaitingQueue.front();
    auto &&ClientToSitIt = mData.find(ClientToSit);

    ProcessOutputEvent(SitDownOutputEvent(Time, ClientToSitIt->first, TableIndexToId(I)));
  } else {
    mTables[I].State = std::nullopt;
  }
}

int main(int argc, char **argv) {
  auto &&Args = CliArguments::ParseArguments(argc, argv);

  std::ifstream IF{Args.Path};
  IF.imbue(std::locale("en_US.utf-8"));
  auto &&C = ParseConfig(IF);
  if (!C) return 0;

  std::cout << std::format("{}", C->Start) << std::endl;

  State S(*C);
  while (!IF.eof()) {
    auto &&IE = ParseInputEvent(IF);
    if (!IE) break;

    std::cout << **IE << std::endl;
    (*IE)->Process(S);
  }

  auto &&D = S.Data();

  std::vector<ClientNameRef> Names;
  Names.reserve(D.size());
  for (auto &&[K, _] : D) {
    Names.emplace_back(K);
  }
  std::ranges::sort(Names);
  for (auto &&K : Names) {
    S.ProcessOutputEvent(LeftOutputEvent(C->End, K));
  }

  std::cout << std::format("{}", C->End) << std::endl;

  auto &&T = S.Tables();
  for (u64 I = 0; I < static_cast<u64>(C->TableCount); ++I) {
    std::cout << I + 1 << " " << T[I].Earned << " " << std::format("{}", T[I].TotalUsed) << std::endl;
  }
  return 0;
}
