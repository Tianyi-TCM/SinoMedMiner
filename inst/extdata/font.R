install.packages("extrafont")
library(extrafont)

font_import()  # 导入系统字体，这一步可能需要一些时间
loadfonts(device = "win")  # Windows 用户使用这条命令

fonts <- fonts()
print(fonts)


windowsFonts(
  KT = windowsFont("楷体"),
  ST = windowsFont("宋体")
)
WindowsFonts()

?windowsFonts()
