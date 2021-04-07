unit sNIF_main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Buttons,
  sNIF.Params, Vcl.Samples.Spin, Vcl.Mask, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.ImageList,
  Vcl.ImgList, Vcl.Menus;

type
  TForm_config = class(TForm)
    PC_main: TPageControl;
    Page_AcercaDe: TTabSheet;
    Image1: TImage;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    Memo1: TMemo;
    StaticText3: TStaticText;
    Page_folders: TTabSheet;
    Page_Attributes: TTabSheet;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Btn_AttrNextPage: TButton;
    Btn_AttrPrevPage: TButton;
    Panel_attr: TPanel;
    Lbl_MinSizeMB: TLabel;
    Label3: TLabel;
    Chk_MinSize: TCheckBox;
    Edit_minSizeKB: TSpinEdit;
    Edit_MinSizeMB: TSpinEdit;
    Panel_limits: TPanel;
    Label12: TLabel;
    Lbl_NoNIFLimit: TLabel;
    Label25: TLabel;
    Edit_MaxAbsNIFCount: TSpinEdit;
    TB_NoNIFLimit: TTrackBar;
    Chk_UseMaxAbsNIFCount: TCheckBox;
    Chk_UseNoNIFLimit: TCheckBox;
    Page_Progress: TTabSheet;
    Label14: TLabel;
    Bevel6: TBevel;
    Btn_Cancel: TButton;
    Btn_ProgressPrevPage: TButton;
    Panel_progress: TPanel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Lbl_FolderCount: TLabel;
    Lbl_TotalFileCount: TLabel;
    Lbl_ExploredFileCount: TLabel;
    Lbl_ReadErrorCount: TLabel;
    Lbl_ElapsedTime: TLabel;
    Lbl_speed: TLabel;
    Label22: TLabel;
    Lbl_Mbytes: TLabel;
    Label23: TLabel;
    Lbl_FilesPerMin: TLabel;
    Bar_progress: TProgressBar;
    Memo_ProgressLog: TMemo;
    MainMenu_sNIF: TMainMenu;
    Menu_Fichero: TMenuItem;
    Menu_CargarCfg: TMenuItem;
    Menu_SalvarCfg: TMenuItem;
    N1: TMenuItem;
    Menu_AcercaDe: TMenuItem;
    Menu_Configurar: TMenuItem;
    Menu_CfgCarpetas: TMenuItem;
    Menu_CfgExtensiones: TMenuItem;
    Menu_CfgAtributos: TMenuItem;
    Menu_CfgParametros: TMenuItem;
    Resultados1: TMenuItem;
    Menu_Ejecutar: TMenuItem;
    Page_Extensiones: TTabSheet;
    Memo_Carpetas: TMemo;
    List_Extensiones: TListBox;
    Panel1: TPanel;
    Label1: TLabel;
    ImgList_botones: TImageList;
    Edit_Extensiones: TButtonedEdit;
    procedure FormCreate(Sender: TObject);
    procedure Page_ExtensionesShow(Sender: TObject);
    procedure Page_AcercaDeShow(Sender: TObject);
    procedure Menu_CfgExtensionesClick(Sender: TObject);
    procedure Menu_CfgCarpetasClick(Sender: TObject);
    procedure Menu_CfgAtributosClick(Sender: TObject);
    procedure Edit_ExtensionesRightButtonClick(Sender: TObject);
  private
    { Private declarations }
    Parametros: TsNIFParametros;
  public
    { Public declarations }
  end;

var
  Form_config: TForm_config;

implementation

{$R *.dfm}

procedure TForm_config.Edit_ExtensionesRightButtonClick(Sender: TObject);
begin
  Parametros.Extensiones.DelimitedText := Edit_Extensiones.Text;
  List_Extensiones.Items.DelimitedText := Parametros.Extensiones.DelimitedText;
end;

procedure TForm_config.FormCreate(Sender: TObject);
var
  I: integer;
begin
  // Inicializar variables internas
  Parametros := TsNIFParametros.Create;

  // Inicializar GUI
  for I := 0 to PC_main.PageCount - 1 do
    PC_main.Pages[I].TabVisible := false;
  PC_main.ActivePageIndex := 0;
  List_Extensiones.Items.Delimiter := ' ';
end;

procedure TForm_config.Menu_CfgAtributosClick(Sender: TObject);
begin
  PC_main.ActivePage := Page_Attributes;
end;

procedure TForm_config.Menu_CfgCarpetasClick(Sender: TObject);
begin
  PC_main.ActivePage := Page_folders;
end;

procedure TForm_config.Menu_CfgExtensionesClick(Sender: TObject);
begin
  PC_main.ActivePage := Page_Extensiones;
end;

procedure TForm_config.Page_AcercaDeShow(Sender: TObject);
begin
  Form_config.Caption := 'sNIF';
end;

procedure TForm_config.Page_ExtensionesShow(Sender: TObject);
begin
  Form_config.Caption := 'Configurar extensiones de fichero';
end;

end.
