{ *******************************************************

  Threading utilities for Delphi 2009 and above

  Utilidad para programación concurrente.

  *******************************************************

  2012-2015 Ángel Fernández Pineda. Madrid. Spain.

  This work is licensed under the Creative Commons
  Attribution-ShareAlike 3.0 Unported License. To
  view a copy of this license,
  visit http://creativecommons.org/licenses/by-sa/3.0/
  or send a letter to Creative Commons,
  444 Castro Street, Suite 900,
  Mountain View, California, 94041, USA.

  ******************************************************* }

unit ThreadUtils.Sync;

{
  SUMMARY:

  - THandleObjectHelper: new primitive MsgWaitFor.

  - TEvent2: extended behavior for TEvent

  - TLockCondition and TPrecondition: lock thread execution until
  some condition is met.

  - TSyncPoint: wait until a number of threads have reached an execution point

  - TRendezvous: pass data to another thread in a synchronized way
}

interface

uses
  SysUtils,
  WinAPI.Windows,
  SyncObjs;

{ THandleObjectHelper

  PURPOUSE:
  To add a VCL-safe WaitFor primitive

  MsgWaitFor:
  Spawns a new thread wich executes WaitFor and then executes
  the given callback on the main thread. This way, message queue may be attended
  in VCL applications
}

type
  TWaitNotifyEvent = procedure(Sender: THandleObject; WaitResult: TWaitResult)
    of object;

  THandleObjectHelper = class helper for THandleObject
  public
    procedure MsgWaitFor(Callback: TWaitNotifyEvent;
      Timeout: cardinal = INFINITE);
  end;

  { TEvent2

    PURPOSE:
    Extend the behavior of TEvent

    GENERAL USAGE:
    Once "Cancel" method is called, all pending threads at "WaitFor" will
    return wrError. Further calls to WaitFor will also return wrError.

    SIGNALANDWAITFOR:
    SignalAndWaitFor reproduces the behavior of SignalObjectAndWait Windows
    API primitive. ObjToSignal parameter should be TSemaphore, TMutex or TEvent.

    PULSEEVENT:
    Will release, at least, all pending threads at the moment.
    No need to use if ManualReset was set to true at instance creation.
    Note: this is not the same as the PulseEvent deprecated Windows primitive.
  }

type
  TEvent2 = class(TEvent)
  protected
    FLockedCounter: integer;
    FCancelled: boolean;
  public
    constructor Create(EventAttributes: PSecurityAttributes;
      ManualReset, InitialState: boolean; const Name: string;
      UseCOMWait: boolean = False); overload;
    constructor Create(UseCOMWait: boolean = False); overload;
    destructor Destroy; override;
    function WaitFor(Timeout: cardinal = INFINITE): TWaitResult; override;
    function SignalAndWaitFor(ObjToSignal: THandleObject;
      Timeout: cardinal = INFINITE): TWaitResult;
    procedure PulseEvent;
    procedure Cancel;
  end;

  { TLockCondition

    PURPOUSE:
    To lock any thread until a certain condition is met.

    GENERAL USAGE:
    - Create a TLockCondition instance
    - enter a critical section
    - Call TLockCondition.WaitFor
    - Leave the critical section

    WAITFOR:
    Call WaitFor to sleep on the given condition and to release the critical
    section specified through the CriticalSection parameter.
    The TimeOut parameter sets the time-out interval.
    When this interval elapses or the condition is met,
    the critical section is reacquired.
    This class ensures that the given condition is allways true after the
    waiting thread has been unlocked and result is wrSignaled (no need for loops).

    WaitFor result is wrError if another thread calls TLockCondition.Cancel
    which also cause the waiting thread to be awaken. Further calls to WaitFor
    will return wrError, too.

    CriticalSection is reacquired as an atomic operation when the locked
    thread is awaken.

    CONDITIONS:
    Conditions are expressed as references to function returning a
    boolean value. Such function should return "true" when
    the unlocking condition is met.

    SIGNALING STATE CHANGES:
    Any thread that changes the state of the WaitFor condition must call
    TLockCondition.Update.

    CANCELLATION:
    TLockCondition.Cancel will unlock all waiting threads at WaitFor with
    wrAbandoned result. Further calls to WaitFor will result in wrAbandoned too.
    Cancellation is provided for smooth instance destruction. Call Cancel,
    wait for threads to terminate, then destroy the instance itself.

    EXAMPLE:

    procedure TMyClass.LockUntilCount(limit: integer);
    begin
    CriticalSection.Enter;
    try
    wr := LockCondition.WaitFor(CriticalSection,
    function:boolean
    begin
    result := (self.counter>=limit)
    end;
    finally
    CriticalSection.Leave;
    end;
    end;

    procedure TMyClass.Inc;
    begin
    CriticalSection.Enter;
    inc(self.counter);
    CriticalSection.Leave;
    LockCondition.Update;
    end;
  }

type
  TLockCondition = class
  private
    FSync: TConditionVariableCS;
    FCancelled: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function WaitFor(CriticalSection: TCriticalSection;
      Condition: TFunc<boolean>; Timeout: cardinal = INFINITE)
      : TWaitResult; overload;
    procedure Update;
    procedure Cancel;
    property Cancelled: boolean read FCancelled;
  end;

  { TPrecondition

    PURPOUSE:
    To lock any thread until a certain condition is met.

    GENERAL USAGE:
    Same as TLockCondition

    CONDITIONS:
    The locking condition is set at instance creation, so it's result should depend
    on your object's state alone. Whether such condition depends on external
    parameters use TLockCondition instead.

    EXAMPLE:

    constructor TMyClass.Create;
    begin
    ...
    NotEmpty := TPrecondition.Create(
    function: boolean
    begin
    Result := (FBuffer.Count>0);
    end);

    NotFull := TPrecondition.Create(
    function: boolean
    begin
    Result := (FBuffer.Count<FBuffer.Size);
    end);
    ...
    end;

    function TmyClass.Consume: TSomeItem;
    begin
    CriticalSection.Enter;
    NotEmpty.WaitFor(CriticalSection);
    Result := FBuffer.Extract;
    CriticalSection.Leave;
    NotEmpty.Update;
    NotFull.Update;
    end;

    procedure TMyClass.Produce(Item: TSomeItem);
    begin
    CriticalSection.Enter;
    NotFull.WaitFor(CriticalSection);
    FBuffer.Add(Item);
    CriticalSection.Leave;
    NotEmpty.Update;
    NotFull.Update;
    end;
  }

type
  TPrecondition = class(TLockCondition)
  private
    FCondition: TFunc<boolean>;
  public
    constructor Create(Condition: TFunc<boolean>);
    function WaitFor(CriticalSection: TCriticalSection;
      Timeout: cardinal = INFINITE): TWaitResult;
  end;

  { TSyncPoint

    PURPOUSE:
    Wait until a number of threads have reached certain point of
    execution. Mostly used for waiting termination of
    all spawned threads, without keeping track of their instance identifiers,
    so shared resources are safe to release after that
    (including the TsyncPoint instance itself)

    GENERAL USAGE:
    - Call AddThread every time a new thread is created
    (or set a value at instance creation)
    - Any thread reaching the required execution point should call Reached
    - Call WaitFor to lock until all other threads meet the sync point.

    NOTES:
    Give every thread a chance to reach the sync point, otherwise a deadlock
    may occur.

    EXAMPLE:

    // assume that "sync" is a shared variable
    sync := TSyncPoint.Create(5)
    for i := 1 to 5 do
    MyThread.Create.Start;
    sync.WaitFor;
    <<release shared resources>>
    sync.Free;

    ...

    procedure MyThread.Execute;
    begin
    <<do something>>
    sync.reached;
    end;
  }

type
  TSyncPoint = class
  private
    FThreadCount: integer;
    Event: TEvent;
  public
    constructor Create(InitialThreadCount: integer = 0);
    destructor Destroy; override;
    procedure AddThread;
    procedure Reached;
    function WaitFor(Timeout: cardinal = INFINITE): TWaitResult;
    procedure MsgWaitFor(Callback: TWaitNotifyEvent);
    property ThreadCount: integer read FThreadCount;
  end;

  { TRendezvous<T>

    PURPOUSE:
    Pass data to another thread in a synchronized way.

    GENERAL USAGE:
    - one thread calls "send" 
    - another thread calls "receive".
    If passed data (of type T) can not be copied by value,
    "receive" should provide a "copy procedure".
    First parameter is the source variable and second parameter is the
    destination variable (also returned by "receive").

    EXAMPLE (Data passed by value):

    R := TRendezvous<integer>.Create;

    // Thread 1
    R.send(89);

    // Thread 2
    intValue := R.receive;

    EXAMPLE (Data passed by reference):

    R := TRendezvous<PByte>.Create;

    // Thread 1
    R.send(buffer);
    buffer[0] := pbyte(8); // this IS SAFE !!!

    // Thread 2
    buffer := R.Receive(
    procedure (source,dest: PByte)
    begin
    CopyMemory(dest,source,BUFFER_SIZE);
    end);

    NOTES:
    - "send" and "receive" are blocking primitives.
    Do not call from the main thread in windows applications
    - "send" and "receive" will unlock pending threads and raise
    an ESyncObjectException after "cancel" is called. Further
    calls will also raise such an exception.
    
  }

type
  TRendezvous<T> = class
  private
    FData: T;
    sendReady, receiveReady: TEvent;
    sendCS: TCriticalSection;
    receiveCS: TCriticalSection;
    FCancelled: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Cancel;
    function receive(copyProcedure: TProc<T, T>): T; overload;
    function receive: T; overload;
    procedure send(data: T);
    property Cancelled: boolean read FCancelled;
  end;

implementation

uses
  Classes;

// -----------------------------------------------------------------------------
// THandleObjectHelper
// -----------------------------------------------------------------------------

// Auxiliary type in order to pass data to auxiliary thread
type
  TMsgWaitData = record
    WaitOn: THandleObject;
    Callback: TWaitNotifyEvent;
    Timeout: cardinal;
  end;

  PMsgWaitData = ^TMsgWaitData;

  // Auxiliary thread body
function MsgWaitForThread(arg: PMsgWaitData): integer;
var
  wr: TWaitResult;
  Callback: TWaitNotifyEvent;
  hObject: THandleObject;
  Timeout: cardinal;
begin
  Callback := arg^.Callback;
  hObject := arg^.WaitOn;
  Timeout := arg^.Timeout;
  FreeMem(arg);
  wr := hObject.WaitFor(Timeout);
  TThread.Synchronize(nil,
    procedure
    begin
      Callback(hObject, wr);
    end);
  result := 0;
end;

// -----------------------------------------------------------------------------

procedure THandleObjectHelper.MsgWaitFor(Callback: TWaitNotifyEvent;
Timeout: cardinal);
var
  th: THandle;
  id: cardinal;
  data: PMsgWaitData;
begin
  if (Assigned(Callback)) then
  begin
    GetMem(data, sizeof(TMsgWaitData));
    data^.WaitOn := self;
    data^.Callback := Callback;
    data^.Timeout := Timeout;
    th := BeginThread(nil, 32, @MsgWaitForThread, data, 0, id);
    if (th <> INVALID_HANDLE_VALUE) then
      CloseHandle(th)
    else
      raise ESyncObjectException.Create('Unable to create auxiliary thread');
  end;
end;

// -----------------------------------------------------------------------------
// TEvent2
// -----------------------------------------------------------------------------

constructor TEvent2.Create(EventAttributes: PSecurityAttributes;
ManualReset, InitialState: boolean; const Name: string;
UseCOMWait: boolean = False);
begin
  FCancelled := False;
  FLockedCounter := 0;
  inherited Create(EventAttributes, ManualReset, InitialState, Name,
    UseCOMWait);
end;

// -----------------------------------------------------------------------------

constructor TEvent2.Create(UseCOMWait: boolean = False);
begin
  FCancelled := False;
  FLockedCounter := 0;
  inherited Create(UseCOMWait);
end;

// -----------------------------------------------------------------------------

destructor TEvent2.Destroy;
begin
  if (FHandle <> 0) then
    Cancel;
  inherited;
end;

// -----------------------------------------------------------------------------

function TEvent2.WaitFor(Timeout: cardinal): TWaitResult;
begin
  if (FCancelled) then
    result := wrError
  else
  begin
    InterlockedIncrement(FLockedCounter);
    result := Inherited WaitFor(Timeout);
    InterlockedDecrement(FLockedCounter);
    if (result = wrSignaled) and FCancelled then
      result := wrError;
  end;
end;

// -----------------------------------------------------------------------------

procedure TEvent2.PulseEvent;
begin
  while (FLockedCounter > 0) do
    SetEvent;
end;

// -----------------------------------------------------------------------------

procedure TEvent2.Cancel;
begin
  FCancelled := True;
  PulseEvent;
end;

// -----------------------------------------------------------------------------

function TEvent2.SignalAndWaitFor(ObjToSignal: THandleObject;
Timeout: cardinal = INFINITE): TWaitResult;
var
  wr: cardinal;
begin
  result := wrError;
  if (not FCancelled) and (ObjToSignal <> nil) then
  begin
    InterlockedIncrement(FLockedCounter);
    wr := SignalObjectAndWait(ObjToSignal.Handle, FHandle, Timeout, False);
    InterlockedDecrement(FLockedCounter);
    if (wr = WAIT_OBJECT_0) and (not FCancelled) then
      result := wrSignaled
    else if (wr = WAIT_TIMEOUT) then
      result := wrTimeout
    else if (wr = WAIT_ABANDONED) then
      result := wrAbandoned;
  end;
end;

// -----------------------------------------------------------------------------
// TLockCondition
// -----------------------------------------------------------------------------

constructor TLockCondition.Create;
begin
  FSync := TConditionVariableCS.Create;
  FCancelled := False;
end;

// -----------------------------------------------------------------------------

destructor TLockCondition.Destroy;
begin
  Cancel;
  FSync.Destroy;
  inherited;
end;

// -----------------------------------------------------------------------------

function TLockCondition.WaitFor(CriticalSection: TCriticalSection;
Condition: TFunc<boolean>; Timeout: cardinal): TWaitResult;
begin
  result := wrSignaled;
  if (Assigned(Condition)) then
    while (not FCancelled) and (result = wrSignaled) and (not Condition()) do
      result := FSync.WaitFor(CriticalSection, Timeout)
  else
    raise ESyncObjectException.Create('TLockCondition: Condition unknown');
  if (FCancelled) then
    result := wrError;
end;

// -----------------------------------------------------------------------------

procedure TLockCondition.Update;
begin
  FSync.ReleaseAll;
end;

// -----------------------------------------------------------------------------

procedure TLockCondition.Cancel;
begin
  FCancelled := True;
  Update;
end;

// -----------------------------------------------------------------------------
// TPrecondition
// -----------------------------------------------------------------------------

constructor TPrecondition.Create(Condition: TFunc<System.boolean>);
begin
  if not Assigned(Condition) then
    raise ESyncObjectException.Create('TPrecondition: precondition unknown');
  FCondition := Condition;
  inherited Create;
end;

// -----------------------------------------------------------------------------

function TPrecondition.WaitFor(CriticalSection: TCriticalSection;
Timeout: cardinal): TWaitResult;
begin
  result := inherited WaitFor(CriticalSection, FCondition, Timeout);
end;


// -----------------------------------------------------------------------------
// TSyncPoint
// -----------------------------------------------------------------------------

constructor TSyncPoint.Create(InitialThreadCount: integer);
begin
  if (InitialThreadCount < 0) then
    InitialThreadCount := 0;
  FThreadCount := InitialThreadCount;
  Event := TEvent.Create(nil, True, (InitialThreadCount = 0), '');
end;

// -----------------------------------------------------------------------------

destructor TSyncPoint.Destroy;
begin
  Event.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TSyncPoint.AddThread;
begin
  if (InterlockedIncrement(FThreadCount) = 0) then
    Event.SetEvent
  else
    Event.ResetEvent;
end;

// -----------------------------------------------------------------------------

procedure TSyncPoint.Reached;
begin
  if (InterlockedDecrement(FThreadCount) = 0) then
    Event.SetEvent;
end;

// -----------------------------------------------------------------------------

function TSyncPoint.WaitFor(Timeout: cardinal): TWaitResult;
begin
  try
    result := Event.WaitFor(Timeout);
  except
    result := wrError;
  end;
end;

// -----------------------------------------------------------------------------

procedure TSyncPoint.MsgWaitFor(Callback: TWaitNotifyEvent);
begin
  Event.MsgWaitFor(Callback);
end;

// -----------------------------------------------------------------------------
// TRendezvous<T>
// -----------------------------------------------------------------------------

constructor TRendezvous<T>.Create;
begin
  sendReady := TEvent.Create(nil, False, False, '');
  receiveReady := TEvent.Create(nil, False, False, '');
  sendCS := TCriticalSection.Create;
  receiveCS := TCriticalSection.Create;
  FCancelled := false;
end;

destructor TRendezvous<T>.Destroy;
begin
  sendReady.Free;
  receiveReady.Free;
  sendCS.Free;
  receiveCS.Free;
end;

procedure TRendezvous<T>.send(data: T);
begin
  sendCS.Enter;
  try
    if (FCancelled) then
      raise ESyncObjectException.Create('Sync object was cancelled');
    FData := data;
    sendReady.SetEvent;
    receiveReady.WaitFor;
  finally
    sendCS.Leave;
  end;
  if (FCancelled) then
    raise ESyncObjectException.Create('Sync object was cancelled');
end;

function TRendezvous<T>.receive: T;
begin
  receiveCS.Enter;
  try
    if (FCancelled) then
      raise ESyncObjectException.Create('Sync object was cancelled');
    sendReady.WaitFor;
    if (not FCancelled) then
    begin
      result := FData;
      receiveReady.SetEvent;
    end;
  finally
    receiveCS.Leave;
  end;
  if (FCancelled) then
    raise ESyncObjectException.Create('Sync object was cancelled');
end;

function TRendezvous<T>.receive(copyProcedure: TProc<T, T>): T;
begin
  receiveCS.Enter;
  try
    if (FCancelled) then
      raise ESyncObjectException.Create('Sync object was cancelled');
    sendReady.WaitFor;
    if (not FCancelled) then
    begin
      result := FData;
      if Assigned(copyProcedure) then
        try
          copyProcedure(FData, result);
        except
        end;
      receiveReady.SetEvent;
    end;
  finally
    receiveCS.Leave;
  end;
  if (FCancelled) then
    raise ESyncObjectException.Create('Sync object was cancelled');
end;

procedure TRendezvous<T>.Cancel;
begin
  FCancelled := True;
  receiveReady.SetEvent;
  sendReady.SetEvent;
end;

// -----------------------------------------------------------------------------

end.
