unit FConfigControls;

{$MODE Delphi}

interface

uses
  LCLProc,LCLIntf, LMessages, SysUtils, Classes, Graphics, Controls, Forms, Types,
  Dialogs, ComCtrls, ToolWin, StdCtrls, UABControlerUI, ExtCtrls,FileUtil;

type
  TConfigControls = class(TForm)
    LATitle: TLabel;
    ToolBar1: TToolBar;
    TBApply: TToolButton;
    TBCancel: TToolButton;
    ListBox: TListBox;
    Shape1: TShape;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TBLoad: TToolButton;
    TBSave: TToolButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure TBCancelClick(Sender: TObject);
    procedure TBApplyClick(Sender: TObject);
    procedure ListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListBoxDblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TBLoadClick(Sender: TObject);
    procedure TBSaveClick(Sender: TObject);
    procedure ListBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FChanged : Boolean;
    FControls : TABUIDualControls;
    procedure SetupDialog(const title : String);
  public
    { Public declarations }
    function Execute(const title : String; const fileName : String) : Boolean; overload;
    function Execute(const title : String; var controls : TABUIDualControls) : Boolean; overload;
  end;

var
  ConfigControls: TConfigControls;

implementation

{$R *.lfm}

uses GLKeyboard, FEnterKey;

// Execute
//
function TConfigControls.Execute(const title : String; const fileName : String) : Boolean;
var
   sl : TStringList;
begin
   OpenDialog.FileName:=fileName;
   OpenDialog.InitialDir:=ExtractFilePath(fileName);
   SaveDialog.FileName:=OpenDialog.FileName;
   SaveDialog.InitialDir:=OpenDialog.InitialDir;
   sl:=TStringList.Create;
   try
      if FileExistsUTF8(fileName) { *Konvertiert von FileExists* } then begin
         sl.LoadFromFile(fileName);
         LoadControls(FControls, sl);
      end else FControls[0]:=cDefaultUIControls;
      SetupDialog(title);

      Result:=(ShowModal=mrOk);

      if Result then begin
         SaveControls(FControls, sl);
         sl.SaveToFile(fileName);
      end;
   finally
      sl.Free;
   end;
end;

// Execute
//
function TConfigControls.Execute(const title : String; var controls : TABUIDualControls) : Boolean;
begin
   FControls:=controls;
   SetupDialog(title);

   Result:=(ShowModal=mrOk);

   if Result then
      controls:=FControls;
end;

// SetupDialog
//
procedure TConfigControls.SetupDialog(const title : String);
var
   i : TABUIControl;
begin
   FChanged:=False;
   if title<>'' then
      LATitle.Caption:=title
   else LATitle.Caption:='Controls Configuration';
   ListBox.Items.Clear;
   for i:=Low(FControls[0]) to High(FControls[0]) do
      ListBox.Items.AddObject('', TObject(i));
end;

procedure TConfigControls.TBCancelClick(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;

procedure TConfigControls.TBApplyClick(Sender: TObject);
begin
   ModalResult:=mrOk;
end;

procedure TConfigControls.ListBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);

   procedure TextCentered(x, y, w : Integer; const txt : String);
   var
      ts : TSize;
   begin
      ts:=ListBox.Canvas.TextExtent(txt);
      ListBox.Canvas.TextOut(x+(w-ts.cx) div 2, y, txt);
   end;

   function NumberOfTimesUsed(vkeyCode : Integer) : Integer;
   var
      i : TABUIControl;
   begin
      Result:=0;
      if vkeyCode=0 then Exit;
      for i:=Low(FControls[0]) to High(FControls[0]) do begin
         if FControls[0][i]=vkeyCode then
            Inc(Result);
         if FControls[1][i]=vkeyCode then
            Inc(Result);
      end;
   end;

var
   ctrl : TABUIControl;
   defColor : TColor;
   listWidth : Integer;
   w : Integer;
begin
   listWidth:=ListBox.Width;//-GetSystemMetrics(SM_CXHTHUMB);
   ctrl:=TABUIControl(ListBox.Items.Objects[Index]);
   w:=3*listWidth div 10;
   with ListBox.Canvas do begin
      {if odSelected in State then begin
         Font.Color:=clNavy;
         defColor:=clWhite;
      end else} begin
         Font.Color:=clBtnText;
         defColor:=clBtnHighlight;
      end;
      Brush.Color:=defColor;
      FillRect(Rect);
      TextOut(2, Rect.Top+2, cUIControlNames[ctrl]);
      if NumberOfTimesUsed(FControls[0][ctrl])>1 then begin
         Brush.Color:=clRed;
         FillRect(Types.Rect(listWidth-2*w, Rect.Top, listWidth-w, Rect.Bottom));
      end;
      TextCentered(listWidth-2*w, Rect.Top+2, w,
                   VirtualKeyCodeToKeyName(FControls[0][ctrl]));
      if NumberOfTimesUsed(FControls[1][ctrl])>1 then begin
         Brush.Color:=clRed;
         FillRect(Types.Rect(listWidth-w, Rect.Top, listWidth, Rect.Bottom));
      end else Brush.Color:=defColor;
      TextCentered(listWidth-w, Rect.Top+2, w,
                   VirtualKeyCodeToKeyName(FControls[1][ctrl]));
      Pen.Color:=clBtnShadow;
      MoveTo(listWidth-2*w, Rect.Top); LineTo(listWidth-2*w, Rect.Bottom);
      MoveTo(listWidth-1*w, Rect.Top); LineTo(listWidth-1*w, Rect.Bottom);
   end;
end;

procedure TConfigControls.ListBoxDblClick(Sender: TObject);
var
   pt : TPoint;
   idx, i : Integer;
   ctrl : TABUIControl;
begin
   GetCursorPos(pt);
   pt:=ListBox.ScreenToClient(pt);
   i:=(pt.X-4*ListBox.Width div 10) div (3*ListBox.Width div 10);
   if (i<0) or (i>1) then Exit;
   idx:=ListBox.ItemIndex;
   if idx>=0 then begin
      ctrl:=TABUIControl(ListBox.Items.Objects[idx]);
      FControls[i][ctrl]:=EnterKey.Execute(cUIControlNames[ctrl]);
      FChanged:=True;
      ListBox.Invalidate;
   end;
end;

procedure TConfigControls.ListBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   pt : TPoint;
   idx, i : Integer;
   ctrl : TABUIControl;
begin
   if Button<>mbRight then Exit;
   GetCursorPos(pt);
   pt:=ListBox.ScreenToClient(pt);
   i:=(pt.X-4*ListBox.Width div 10) div (3*ListBox.Width div 10);
   if (i<0) or (i>1) then Exit;
   idx:=ListBox.ItemIndex;
   if idx>=0 then begin
      ctrl:=TABUIControl(ListBox.Items.Objects[idx]);
      FControls[i][ctrl]:=0;
      FChanged:=True;
      ListBox.Invalidate;
   end;
end;

procedure TConfigControls.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
   CanClose:=True;
   if (ModalResult=mrCancel) and FChanged then
      if MessageDlg('Abandon changes?', mtConfirmation, [mbYes, mbNo], 0)<>mrYes then
         CanClose:=False;
end;

procedure TConfigControls.TBLoadClick(Sender: TObject);
var
   sl : TStringList;
begin
   if OpenDialog.Execute then begin
      SaveDialog.FileName:=OpenDialog.FileName;
      sl:=TStringList.Create;
      try
         sl.LoadFromFile(OpenDialog.FileName);
         LoadControls(FControls, sl);
      finally
         sl.Free;
      end;
   end;
end;

procedure TConfigControls.TBSaveClick(Sender: TObject);
var
   sl : TStringList;
begin
   if SaveDialog.Execute then begin
      sl:=TStringList.Create;
      try
         SaveControls(FControls, sl);
         sl.SaveToFile(SaveDialog.FileName);
      finally
         sl.Free;
      end;
   end;
end;

end.
