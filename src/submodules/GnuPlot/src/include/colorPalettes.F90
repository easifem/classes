SELECT CASE (LowerCase(TRIM(ADJUSTL(paletteName))))
CASE ('set1')
  pltname = 'set1'
  palette(1:maxcolors) = ["#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", &
                          "#FF7F00", "#FFFF33", "#A65628", "#F781BF"]
CASE ('set2')
  pltname = 'set2'
  palette(1:maxcolors) = ["#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", &
                          "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3"]
CASE ('set3')
  pltname = 'set3'
  palette(1:maxcolors) = ["#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", &
                          "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5"]
CASE ('palette1')
  pltname = 'palette1'
  palette(1:maxcolors) = ["#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", &
                          "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC"]
CASE ('palette2')
  pltname = 'palette2'
  palette(1:maxcolors) = ["#B3E2CD", "#FDCDAC", "#CDB5E8", "#F4CAE4", &
                          "#D6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC"]
CASE ('paired')
  pltname = 'paired'
  palette(1:maxcolors) = ["#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", &
                          "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00"]
CASE ('dark2')
  pltname = 'dark2'
  palette(1:maxcolors) = ["#1B9E77", "#D95F02", "#7570B3", "#E7298A", &
                          "#66A61E", "#E6AB02", "#A6761D", "#666666"]
CASE ('accent')
  pltname = 'accent'
  palette(1:maxcolors) = ["#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", &
                          "#386CB0", "#F0027F", "#BF5B17", "#666666"]
CASE ('jet')
  maxcolors = 9
  pltname = 'jet'
  palette(1:maxcolors) = ['#000090', '#000fff', '#0090ff', '#0fffee', &
                        '#90ff70', '#ffee00', '#ff7000', '#ee0000', '#7f0000']
CASE ('vik')
  maxcolors = 10
  pltname = 'vik'
  palette(1:maxcolors) = ["#001261", "#033E7D", "#1E6F9D", "#71A8C4", "#C9DDE7", &
                        "#EACEBD", "#D39774", "#BE6533", "#8B2706", "#590008"]

CASE default
  PRINT *, modName//": color_palettes: wrong palette name"
  PRINT *, 'gnuplot default palette will be used!'
  paletteScript = ' ' ! empty palette is returned!
  RETURN
END SELECT
