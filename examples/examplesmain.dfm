object Form1: TForm1
  Left = 472
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Fit Grids Examples'
  ClientHeight = 678
  ClientWidth = 921
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 78
    Height = 13
    Caption = 'TColorStringGrid'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label2: TLabel
    Left = 8
    Top = 232
    Width = 63
    Height = 13
    Caption = 'TNumericGrid'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label3: TLabel
    Left = 464
    Top = 455
    Width = 62
    Height = 13
    Caption = 'TColoredGrid'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label4: TLabel
    Left = 464
    Top = 8
    Width = 43
    Height = 13
    Caption = 'TIDAGrid'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label5: TLabel
    Left = 464
    Top = 232
    Width = 48
    Height = 13
    Caption = 'TDataGrid'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label6: TLabel
    Left = 8
    Top = 456
    Width = 44
    Height = 13
    Caption = 'TGEFGrid'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label7: TLabel
    Left = 8
    Top = 32
    Width = 331
    Height = 13
    Caption = 
      'Grid allows setting up colors of different types of cells at des' +
      'ign time.'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label8: TLabel
    Left = 8
    Top = 480
    Width = 336
    Height = 13
    Caption = 
      'Grid provides handler for end of editing event with coordinates ' +
      'of cell.'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label9: TLabel
    Left = 8
    Top = 256
    Width = 316
    Height = 13
    Caption = 
      'Grid allows to filter input characters based on assigned data ty' +
      'pe.'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label10: TLabel
    Left = 464
    Top = 256
    Width = 301
    Height = 13
    Caption = 'Grid is fully controlled by object implementing IGridDataSource.'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label11: TLabel
    Left = 464
    Top = 480
    Width = 334
    Height = 13
    Caption = 
      'Colors of cells are controlled by object implementing IGridDataS' +
      'ource.'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label12: TLabel
    Left = 464
    Top = 32
    Width = 383
    Height = 13
    Caption = 
      'Grid providing insert, delete, add operations (right click to se' +
      'lect the operation).'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object ColorStringGrid1: TColorStringGrid
    Left = 8
    Top = 56
    Width = 450
    Height = 166
    ColCount = 5
    RowCount = 5
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 0
    OddRowColor = clWhite
    EvenRowColor = clAqua
    SelectedRegionColor = clYellow
    ColNumFixed = False
    RowNumFixed = False
    ColWidths = (
      64
      134
      64
      64
      64)
  end
  object NumericGrid1: TNumericGrid
    Left = 8
    Top = 280
    Width = 450
    Height = 166
    ColCount = 6
    RowCount = 5
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 1
    OddRowColor = clWhite
    EvenRowColor = clYellow
    SelectedRegionColor = 2790064
    ColNumFixed = False
    RowNumFixed = False
    DisabledColor = clGray
  end
  object GEFGrid1: TGEFGrid
    Left = 8
    Top = 504
    Width = 450
    Height = 166
    ColCount = 5
    RowCount = 5
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 2
    Modified = False
    OnGridEditingFinished = GEFGrid1GridEditingFinished
  end
  object IDAGrid1: TIDAGrid
    Left = 464
    Top = 56
    Width = 450
    Height = 166
    ColCount = 5
    RowCount = 5
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    PopupMenu = PopupMenuIDAGrid
    TabOrder = 3
    Modified = False
    ColNumFixed = False
    RowNumFixed = False
    Changeable = True
  end
  object DataGrid1: TDataGrid
    Left = 464
    Top = 280
    Width = 450
    Height = 166
    ColCount = 5
    RowCount = 5
    TabOrder = 4
    Modified = False
    ColNumFixed = False
    RowNumFixed = False
    Changeable = True
  end
  object ColoredGrid1: TColoredGrid
    Left = 464
    Top = 504
    Width = 450
    Height = 166
    ColCount = 5
    RowCount = 5
    TabOrder = 5
    Modified = False
    ColNumFixed = False
    RowNumFixed = False
    Changeable = True
    OddRowColor = clWhite
    EvenRowColor = clYellow
    SelectedRegionColor = 2790064
    DisabledColor = clBlack
  end
  object BitBtnColorStringGridCopy: TBitBtn
    Left = 384
    Top = 17
    Width = 35
    Height = 30
    Glyph.Data = {
      36050000424D3605000000000000360400002800000010000000100000000100
      08000000000000010000C40E0000C40E00000001000000000000000000004000
      000080000000FF000000002000004020000080200000FF200000004000004040
      000080400000FF400000006000004060000080600000FF600000008000004080
      000080800000FF80000000A0000040A0000080A00000FFA0000000C0000040C0
      000080C00000FFC0000000FF000040FF000080FF0000FFFF0000000020004000
      200080002000FF002000002020004020200080202000FF202000004020004040
      200080402000FF402000006020004060200080602000FF602000008020004080
      200080802000FF80200000A0200040A0200080A02000FFA0200000C0200040C0
      200080C02000FFC0200000FF200040FF200080FF2000FFFF2000000040004000
      400080004000FF004000002040004020400080204000FF204000004040004040
      400080404000FF404000006040004060400080604000FF604000008040004080
      400080804000FF80400000A0400040A0400080A04000FFA0400000C0400040C0
      400080C04000FFC0400000FF400040FF400080FF4000FFFF4000000060004000
      600080006000FF006000002060004020600080206000FF206000004060004040
      600080406000FF406000006060004060600080606000FF606000008060004080
      600080806000FF80600000A0600040A0600080A06000FFA0600000C0600040C0
      600080C06000FFC0600000FF600040FF600080FF6000FFFF6000000080004000
      800080008000FF008000002080004020800080208000FF208000004080004040
      800080408000FF408000006080004060800080608000FF608000008080004080
      800080808000FF80800000A0800040A0800080A08000FFA0800000C0800040C0
      800080C08000FFC0800000FF800040FF800080FF8000FFFF80000000A0004000
      A0008000A000FF00A0000020A0004020A0008020A000FF20A0000040A0004040
      A0008040A000FF40A0000060A0004060A0008060A000FF60A0000080A0004080
      A0008080A000FF80A00000A0A00040A0A00080A0A000FFA0A00000C0A00040C0
      A00080C0A000FFC0A00000FFA00040FFA00080FFA000FFFFA0000000C0004000
      C0008000C000FF00C0000020C0004020C0008020C000FF20C0000040C0004040
      C0008040C000FF40C0000060C0004060C0008060C000FF60C0000080C0004080
      C0008080C000FF80C00000A0C00040A0C00080A0C000FFA0C00000C0C00040C0
      C00080C0C000FFC0C00000FFC00040FFC00080FFC000FFFFC0000000FF004000
      FF008000FF00FF00FF000020FF004020FF008020FF00FF20FF000040FF004040
      FF008040FF00FF40FF000060FF004060FF008060FF00FF60FF000080FF004080
      FF008080FF00FF80FF0000A0FF0040A0FF0080A0FF00FFA0FF0000C0FF0040C0
      FF0080C0FF00FFC0FF0000FFFF0040FFFF0080FFFF00FFFFFF00E3E3E3E3E3E3
      E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3
      E3020202020202020202E3E3E3E3E3E3E302FFFFFFFFFFFFFF02E3E3E3E3E3E3
      E302FF0000000000FF02E300000000000002FFFFFFFFFFFFFF02E300FFFFFFFF
      FF02FF0000000000FF02E300FF0000000002FFFFFFFFFFFFFF02E300FFFFFFFF
      FF02FF0000FF02020202E300FF0000000002FFFFFFFF02FF02E3E300FFFFFFFF
      FF02FFFFFFFF0202E3E3E300FF0000FF00020202020202E3E3E3E300FFFFFFFF
      00FF00E3E3E3E3E3E3E3E300FFFFFFFF0000E3E3E3E3E3E3E3E3E30000000000
      00E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3}
    TabOrder = 6
    OnClick = BitBtnColorStringGridCopyClick
  end
  object BitBtnColorStringGridPaste: TBitBtn
    Left = 423
    Top = 17
    Width = 35
    Height = 30
    Glyph.Data = {
      36050000424D3605000000000000360400002800000010000000100000000100
      08000000000000010000C40E0000C40E00000001000000000000000000004000
      000080000000FF000000002000004020000080200000FF200000004000004040
      000080400000FF400000006000004060000080600000FF600000008000004080
      000080800000FF80000000A0000040A0000080A00000FFA0000000C0000040C0
      000080C00000FFC0000000FF000040FF000080FF0000FFFF0000000020004000
      200080002000FF002000002020004020200080202000FF202000004020004040
      200080402000FF402000006020004060200080602000FF602000008020004080
      200080802000FF80200000A0200040A0200080A02000FFA0200000C0200040C0
      200080C02000FFC0200000FF200040FF200080FF2000FFFF2000000040004000
      400080004000FF004000002040004020400080204000FF204000004040004040
      400080404000FF404000006040004060400080604000FF604000008040004080
      400080804000FF80400000A0400040A0400080A04000FFA0400000C0400040C0
      400080C04000FFC0400000FF400040FF400080FF4000FFFF4000000060004000
      600080006000FF006000002060004020600080206000FF206000004060004040
      600080406000FF406000006060004060600080606000FF606000008060004080
      600080806000FF80600000A0600040A0600080A06000FFA0600000C0600040C0
      600080C06000FFC0600000FF600040FF600080FF6000FFFF6000000080004000
      800080008000FF008000002080004020800080208000FF208000004080004040
      800080408000FF408000006080004060800080608000FF608000008080004080
      800080808000FF80800000A0800040A0800080A08000FFA0800000C0800040C0
      800080C08000FFC0800000FF800040FF800080FF8000FFFF80000000A0004000
      A0008000A000FF00A0000020A0004020A0008020A000FF20A0000040A0004040
      A0008040A000FF40A0000060A0004060A0008060A000FF60A0000080A0004080
      A0008080A000FF80A00000A0A00040A0A00080A0A000FFA0A00000C0A00040C0
      A00080C0A000FFC0A00000FFA00040FFA00080FFA000FFFFA0000000C0004000
      C0008000C000FF00C0000020C0004020C0008020C000FF20C0000040C0004040
      C0008040C000FF40C0000060C0004060C0008060C000FF60C0000080C0004080
      C0008080C000FF80C00000A0C00040A0C00080A0C000FFA0C00000C0C00040C0
      C00080C0C000FFC0C00000FFC00040FFC00080FFC000FFFFC0000000FF004000
      FF008000FF00FF00FF000020FF004020FF008020FF00FF20FF000040FF004040
      FF008040FF00FF40FF000060FF004060FF008060FF00FF60FF000080FF004080
      FF008080FF00FF80FF0000A0FF0040A0FF0080A0FF00FFA0FF0000C0FF0040C0
      FF0080C0FF00FFC0FF0000FFFF0040FFFF0080FFFF00FFFFFF00E3E3E3E3E3E3
      E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E302020202020202020202E30000000000
      02FFFFFFFFFFFFFFFF0200929092909202FF000000000000FF02009092909290
      02FFFFFFFFFFFFFFFF0200929092909202FF000000FF02020202009092909290
      02FFFFFFFFFF02FF02E300929092909202FFFFFFFFFF0202E3E3009092909290
      0202020202020200E3E30092909290929092909290929000E3E3009092000000
      0000000000929200E3E300929200E3E3E3E3E3E300929000E3E30090929000FC
      0000FC0092909200E3E3E30000000000FCFC0000000000E3E3E3E3E3E3E3E300
      000000E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3}
    TabOrder = 7
    OnClick = BitBtnColorStringGridPasteClick
  end
  object PopupMenuIDAGrid: TPopupMenu
    Left = 832
    Top = 168
    object MenuItemAddColumn: TMenuItem
      Caption = 'Add Column'
      OnClick = MenuItemAddColumnClick
    end
    object MenuItemDeleteColumn: TMenuItem
      Caption = 'Delete Column'
      OnClick = MenuItemDeleteColumnClick
    end
    object MenuItemAddRow: TMenuItem
      Caption = 'Add Row'
      OnClick = MenuItemAddRowClick
    end
    object MenuItemDeleteRow: TMenuItem
      Caption = 'Delete Row'
      OnClick = MenuItemDeleteRowClick
    end
    object MenuItemInsertColumn: TMenuItem
      Caption = 'Insert Column'
      OnClick = MenuItemInsertColumnClick
    end
    object MenuItemInsertRow: TMenuItem
      Caption = 'Insert Row'
      OnClick = MenuItemInsertRowClick
    end
  end
end
