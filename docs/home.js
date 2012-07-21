$(document).ready(function(){
    $('.language-javascript').each(function(){
        $(this).text(js_beautify($(this).text(),
                                 { indent_size: 2}));
    });
    hljs.tabReplace = '    ';
    hljs.initHighlightingOnLoad();
    $('.example').each(function(){
        var example = $(this);
        var tabs = $('<div></div>');
        example.children('.lang').each(function(){
            var title = $(this).clone().click(function(){
                example.find('.pre').hide();
                pre.show();
                tabs.children().removeClass('tab-title-current');
                title.addClass('tab-title-current');
            });
            title.addClass('tab-title');
            var pre = $(this).next();
            tabs.append(title);
        }).remove();
        example.prepend(tabs);
        example.find('.pre').hide().first().show();
        tabs.children().first().addClass('tab-title-current');
    });
});
