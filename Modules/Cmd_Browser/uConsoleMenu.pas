unit uConsoleMenu;

interface

uses SysUtils;

type
  TMenuAction = procedure of object;

  { Base class for menu items }
  TMenuElement = class
  public
    Caption: string;
    constructor Create(const ACaption: string);
    procedure Execute; virtual; abstract;
  end;

  { Composite class representing a Menu containing other elements }
  TConsoleMenu = class(TMenuElement)
  private
    FItems: array of TMenuElement;
    FIsRoot: Boolean;
    procedure ClearScreen;
  public
    constructor Create(const ACaption: string; IsRoot: Boolean = False);
    destructor Destroy; override;
    procedure Add(AItem: TMenuElement);
    procedure Execute; override;
  end;

  { Leaf class representing a selectable action }
  TMenuItem = class(TMenuElement)
  private
    FOnSelect: TMenuAction;
  public
    constructor Create(const ACaption: string; AOnSelect: TMenuAction);
    procedure Execute; override;
  end;

  // Callback that returns an array of strings (the names)
  TGetListEvent = function: TArray<string> of object;
  // Callback that handles the name selection
  TNameSelectEvent = procedure(const SelectedName: string) of object;

  TDynamicNameMenu = class(TConsoleMenu)
  private
    FOnGetList: TGetListEvent;
    FOnNameSelect: TNameSelectEvent;
    procedure HandleAction;
  public
    constructor Create(const ACaption: string; AGetList: TGetListEvent; AOnSelect: TNameSelectEvent);
    procedure Execute; override;
  end;


implementation

{ TMenuElement }

constructor TMenuElement.Create(const ACaption: string);
begin
  Caption := ACaption;
end;

{ TConsoleMenu }

constructor TConsoleMenu.Create(const ACaption: string; IsRoot: Boolean = False);
begin
  inherited Create(ACaption);
  FIsRoot := IsRoot;
  SetLength(FItems, 0);
end;

destructor TConsoleMenu.Destroy;
var
  I: Integer;
begin
  for I := Low(FItems) to High(FItems) do
    FItems[I].Free;
  inherited;
end;

procedure TConsoleMenu.Add(AItem: TMenuElement);
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[High(FItems)] := AItem;
end;

procedure TConsoleMenu.ClearScreen;
begin
  // Standard VT100 escape code to clear terminal and reset cursor
  Write(#27'[2J'#27'[H');
end;

procedure TConsoleMenu.Execute;
var
  Input: string;
  Choice, I: Integer;
  ShouldExit: Boolean;
begin
  ShouldExit := False;
  repeat
    ClearScreen;
    Writeln('=== ', Caption, ' ===');
    for I := Low(FItems) to High(FItems) do
      Writeln(Format('%d. %s', [I + 1, FItems[I].Caption]));

    if FIsRoot then
      Writeln('0. Exit Application')
    else
      Writeln('0. Back');

    Write('> ');
    Readln(Input);

    if TryStrToInt(Input, Choice) then
    begin
      if Choice = 0 then
        ShouldExit := True
      else if (Choice > 0) and (Choice <= Length(FItems)) then
        FItems[Choice - 1].Execute
      else
        begin
          Writeln('Invalid selection. Press Enter.');
          Readln;
        end;
    end;
  until ShouldExit;
end;

{ TMenuItem }

constructor TMenuItem.Create(const ACaption: string; AOnSelect: TMenuAction);
begin
  inherited Create(ACaption);
  FOnSelect := AOnSelect;
end;

procedure TMenuItem.Execute;
begin
  if Assigned(FOnSelect) then
    FOnSelect;
end;

constructor TDynamicNameMenu.Create(const ACaption: string; AGetList: TGetListEvent; AOnSelect: TNameSelectEvent);
begin
  inherited Create(ACaption, False);
  FOnGetList := AGetList;
  FOnNameSelect := AOnSelect;
end;

{ Add this to the implementation section of your unit }

procedure TDynamicNameMenu.HandleAction;
//var
  // We'll peek at what the user just selected in the menu loop
  // This is a placeholder for the logic inside the Execute loop
begin
  // Implementation details below...
end;

procedure TDynamicNameMenu.Execute;
var
  Names: TArray<string>;
  N: string;
  I: Integer;
  Choice: Integer;
  Input: string;
begin
  // 1. CLEAR existing items
  for I := Low(FItems) to High(FItems) do FItems[I].Free;
  SetLength(FItems, 0);

  // 2. REBUILD list from the callback
  if Assigned(FOnGetList) then
  begin
    Names := FOnGetList();
    for N in Names do
      Add(TMenuItem.Create(N, nil)); // We pass nil because we handle it locally
  end;

  // 3. DISPLAY & SELECT (Modified version of parent loop)
  repeat
    ClearScreen;
    Writeln('=== ', Caption, ' ===');
    for I := Low(FItems) to High(FItems) do
      Writeln(Format('%d. %s', [I + 1, FItems[I].Caption]));
    Writeln('0. Back');
    Write('> ');
    Readln(Input);

    if TryStrToInt(Input, Choice) then
    begin
      if Choice = 0 then Exit; // Go back

      if (Choice > 0) and (Choice <= Length(FItems)) then
      begin
        // This is where HandleAction logic lives:
        if Assigned(FOnNameSelect) then
          FOnNameSelect(FItems[Choice - 1].Caption); // Pass the Name back to your app
      end;
    end;
  until False;
end;




// Note: To make TMenuItem work with dynamic names, update TConsoleMenu.Execute
// to handle cases where FOnSelect is nil but we want to return the Caption.


end.

