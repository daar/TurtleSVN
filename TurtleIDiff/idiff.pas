unit IDiff;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, ComCtrls;

type

  { TIDiffForm }

  TIDiffForm = class(TForm)
    FilenameLabel: TLabel;
    LeftImageLabel: TLabel;
    PaintBox: TPaintBox;
    CompareMethodPanel: TPanel;
    BlendRadioButton: TRadioButton;
    FlipRadioButton: TRadioButton;
    DifferenceRadioButton: TRadioButton;
    RightImageLabel: TLabel;
    LeftImage: TImage;
    CompareTopPanel: TPanel;
    ImageDetailListView: TListView;
    PageControl1: TPageControl;
    RightImagePanel: TPanel;
    LeftImagePanel: TPanel;
    RightImage: TImage;
    StatusBar1: TStatusBar;
    CompareTabSheet: TTabSheet;
    SideBySideTabSheet: TTabSheet;
    ImageInfoTabSheet: TTabSheet;
    FlipTimer: TTimer;
    BlendTrackBar: TTrackBar;
    procedure BlendRadioButtonChange(Sender: TObject);
    procedure BlendTrackBarChange(Sender: TObject);
    procedure DifferenceRadioButtonChange(Sender: TObject);
    procedure FlipRadioButtonChange(Sender: TObject);
    procedure FlipTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure SideBySideTabSheetShow(Sender: TObject);
  private
    DiffBitmap: TBitmap;
    BlendBitmap: TBitmap;
    LeftVisible: boolean;
    FLeftImgFile: string;
    FRightImgFile: string;
    FLeftImgBmp: TBitmap;
    FRightImgBmp: TBitmap;

    procedure SetDiffBmp;
    procedure SetBlendBmp(BlendValue: double);
    procedure SetLeftImgFile(AValue: string);
    procedure SetRightImgFile(AValue: string);
    { private declarations }
  public
    { public declarations }
    property LeftImgFile: string read FLeftImgFile write SetLeftImgFile;
    property RightImgFile: string read FRightImgFile write SetRightImgFile;

    procedure Initialize({%H-}Data: PtrInt);
    function PixelFormatStr(ABitmap: TBitmap): string;
  end;

var
  IDiffForm: TIDiffForm;

procedure ShowIDiffForm(ALeftImgFile, ARightImgFile: string);

implementation

{$R *.lfm}

uses
  Math, Utils, FPImage, IntfGraphics, LCLType;

procedure ShowIDiffForm(ALeftImgFile, ARightImgFile: string);
begin
  if not Assigned(IDiffForm) then
    IDiffForm := TIDiffForm.Create(nil);

  IDiffForm.LeftImgFile := ALeftImgFile;
  IDiffForm.RightImgFile := ARightImgFile;
  IDiffForm.Show;
end;

{ TIDiffForm }

procedure TIDiffForm.FormCreate(Sender: TObject);
begin
  LeftImagePanel.Width := SideBySideTabSheet.Width div 2;

  Application.QueueAsyncCall(@Initialize, 0);
end;

procedure TIDiffForm.FlipTimerTimer(Sender: TObject);
begin
  PaintBox.Refresh;
end;

procedure TIDiffForm.SetDiffBmp;
var
  RightIntfImg: TLazIntfImage;
  LeftIntfImg: TLazIntfImage;
  ImgHandle: HBitmap;
  ImgMaskHandle: HBitmap;
  px, py: integer;
  RightColor: TFPColor;
  LeftColor: TFPColor;
begin
  RightIntfImg := TLazIntfImage.Create(0, 0);
  RightIntfImg.LoadFromBitmap(FRightImgBmp.Handle, FRightImgBmp.MaskHandle);

  LeftIntfImg := TLazIntfImage.Create(0, 0);
  LeftIntfImg.LoadFromBitmap(FLeftImgBmp.Handle, FLeftImgBmp.MaskHandle);

  //loop through each pixel in the right image
  for py := 0 to RightIntfImg.Height - 1 do
  begin
    for px := 0 to RightIntfImg.Width - 1 do
    begin
      RightColor := RightIntfImg.Colors[px, py];
      LeftColor := LeftIntfImg.Colors[px, py];

      RightColor.Red := Min(Max(0, LeftColor.Red - RightColor.Red), $ffff);
      RightColor.Green := Min(Max(0, LeftColor.Green - RightColor.Green), $ffff);
      RightColor.Blue := Min(Max(0, LeftColor.Blue - RightColor.Blue), $ffff);
      LeftIntfImg.Colors[px, py] := RightColor;
    end;
  end;
  LeftIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  DiffBitmap.Handle := ImgHandle;
  DiffBitmap.MaskHandle := ImgMaskHandle;

  RightIntfImg.Free;
  LeftIntfImg.Free;
end;

procedure TIDiffForm.SetBlendBmp(BlendValue: double);
var
  RightIntfImg: TLazIntfImage;
  LeftIntfImg: TLazIntfImage;
  ImgHandle: HBitmap;
  ImgMaskHandle: HBitmap;
  px, py: integer;
  RightColor: TFPColor;
  LeftColor: TFPColor;
  InvBlendValue: double;
begin
  RightIntfImg := TLazIntfImage.Create(0, 0);
  RightIntfImg.LoadFromBitmap(FRightImgBmp.Handle, FRightImgBmp.MaskHandle);

  LeftIntfImg := TLazIntfImage.Create(0, 0);
  LeftIntfImg.LoadFromBitmap(FLeftImgBmp.Handle, FLeftImgBmp.MaskHandle);

  InvBlendValue := 1 - BlendValue;

  //loop through each pixel in the right image
  for py := 0 to RightIntfImg.Height - 1 do
  begin
    for px := 0 to RightIntfImg.Width - 1 do
    begin
      RightColor := RightIntfImg.Colors[px, py];
      LeftColor := LeftIntfImg.Colors[px, py];

      RightColor.Red := Trunc(LeftColor.Red * InvBlendValue + RightColor.Red * BlendValue);
      RightColor.Green := Trunc(LeftColor.Green * InvBlendValue + RightColor.Green * BlendValue);
      RightColor.Blue := Trunc(LeftColor.Blue * InvBlendValue + RightColor.Blue * BlendValue);
      LeftIntfImg.Colors[px, py] := RightColor;
    end;
  end;
  LeftIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  BlendBitmap.Handle := ImgHandle;
  BlendBitmap.MaskHandle := ImgMaskHandle;

  RightIntfImg.Free;
  LeftIntfImg.Free;
end;

procedure TIDiffForm.DifferenceRadioButtonChange(Sender: TObject);
begin
  FlipTimer.Enabled := False;
  PaintBox.Refresh;
end;

procedure TIDiffForm.BlendRadioButtonChange(Sender: TObject);
begin
  FlipTimer.Enabled := False;
  PaintBox.Refresh;
end;

procedure TIDiffForm.BlendTrackBarChange(Sender: TObject);
begin
  SetBlendBmp(BlendTrackBar.Position / 100);
  PaintBox.Refresh;
end;

procedure TIDiffForm.FormResize(Sender: TObject);
begin
  LeftImagePanel.Width := SideBySideTabSheet.Width div 2;
end;

procedure TIDiffForm.PaintBoxPaint(Sender: TObject);
begin
  //compare method

  if BlendRadioButton.Checked then
  begin
    PaintBox.Canvas.Draw(0, 0, BlendBitmap);
  end;

  if FlipRadioButton.Checked then
  begin
    if LeftVisible then
    begin
      PaintBox.Canvas.Draw(0, 0, FLeftImgBmp);
      FilenameLabel.Caption := LeftImgFile;
    end
    else
    begin
      PaintBox.Canvas.Draw(0, 0, FRightImgBmp);
      FilenameLabel.Caption := RightImgFile;
    end;
    LeftVisible := not LeftVisible;
  end;

  if DifferenceRadioButton.Checked then
  begin
    PaintBox.Canvas.Draw(0, 0, DiffBitmap);
  end;
end;

procedure TIDiffForm.SideBySideTabSheetShow(Sender: TObject);
begin
  LeftImagePanel.Width := SideBySideTabSheet.Width div 2;
end;

procedure TIDiffForm.SetLeftImgFile(AValue: string);
begin
  if FLeftImgFile = AValue then
    Exit;
  FLeftImgFile := AValue;

  LeftImage.Picture.LoadFromFile(AValue);
  FLeftImgBmp := LeftImage.Picture.Bitmap;
  LeftImageLabel.Caption := AValue;
end;

procedure TIDiffForm.SetRightImgFile(AValue: string);
begin
  if FRightImgFile = AValue then
    Exit;
  FRightImgFile := AValue;

  RightImage.Picture.LoadFromFile(AValue);
  FRightImgBmp := RightImage.Picture.Bitmap;
  RightImageLabel.Caption := AValue;
end;

procedure TIDiffForm.FlipRadioButtonChange(Sender: TObject);
begin
  FlipTimer.Enabled := True;
end;

procedure TIDiffForm.Initialize(Data: PtrInt);
var
  li: TListItem;
begin
  ImageDetailListView.Clear;

  li := ImageDetailListView.Items.Add;
  li.Caption := 'File size';
  li.SubItems.Add(ConvertBytes(FileSize(LeftImgFile)));
  li.SubItems.Add(ConvertBytes(FileSize(RightImgFile)));

  li := ImageDetailListView.Items.Add;
  li.Caption := 'Width';
  li.SubItems.Add(IntToStr(FLeftImgBmp.Width) + ' px');
  li.SubItems.Add(IntToStr(FRightImgBmp.Width) + ' px');

  li := ImageDetailListView.Items.Add;
  li.Caption := 'Height';
  li.SubItems.Add(IntToStr(FLeftImgBmp.Height) + ' px');
  li.SubItems.Add(IntToStr(FRightImgBmp.Height) + ' px');

  li := ImageDetailListView.Items.Add;
  li.Caption := 'Pixelformat';
  li.SubItems.Add(PixelFormatStr(FLeftImgBmp));
  li.SubItems.Add(PixelFormatStr(FRightImgBmp));

  DiffBitmap := TBitmap.Create;
  SetDiffBmp;

  BlendBitmap := TBitmap.Create;
  SetBlendBmp(BlendTrackBar.Position / 100);

  PaintBox.Refresh;
end;

function TIDiffForm.PixelFormatStr(ABitmap: TBitmap): string;
begin
  case ABitmap.PixelFormat of
    pfDevice: Result := 'Device';
    pf1bit: Result := '1 bit';
    pf4bit: Result := '4 bit';
    pf8bit: Result := '8 bit';
    pf15bit: Result := '15 bit';
    pf16bit: Result := '16 bit';
    pf24bit: Result := '24 bit';
    pf32bit: Result := '32 bit';
    pfCustom: Result := 'Custom';
  end;
end;

end.
