project: easifemClasses
summary: EASIFEM Library
project_download: https://github.com/vickysharma0812
project_github: https://github.com/vickysharma0812
project_website: http://www.easifem.com
license: gfdl
project_dir: ./src
media_dir: ./media
page_dir: ${pages}/docs/easifemClasses
output_dir: ./docs
exclude_dir: ./src/submodules/
author: Vikas Sharma
author_description: Ph. D.
	Graduate School of Agriculture, Kyoto University,
	Kyoto, Japan
email: vickysharma0812@gmail.com
github: https://vickysharma0812.github.io/
author_pic:
twitter:
website:
graph: false #time consuming
source: false #time consuming
display: public
         protected
         private
page: true
sort: alpha
coloured_edges: true
extra_filetypes:  inc !
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
               markdown.extensions.extra
predocmark_alt: >
predocmark: <
docmark_alt: *
docmark: !
fpp_extensions: f90
preprocesses: true

{!./README.md!}

<!-- FORD features two macros to make it easier to provide intradocumentation links. These are `|url|` which gets replaced by the project URL, and `|media|`, which gets replaced by the (absolute) path to the media directory in the output. you can also use `favicon:` -->
