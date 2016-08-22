for f in *.Rmd; do
    name=`echo $f | cut -f1 -d.`
    Rscript -e "require(knitr); require(markdown); knit('${name}.Rmd', '${name}.md'); markdownToHTML('${name}.md', '${name}.html', options=c('use_xhtml', 'base64_images'))";
done
