$(document).ready(function(){
    $('.language-javascript').each(function(){
        $(this).text(js_beautify($(this).text(),
                                { indent_size: 2
                                }));
    });
    hljs.tabReplace = '    ';
    hljs.initHighlightingOnLoad();
});
