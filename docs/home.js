$(document).ready(function(){
    $('.language-javascript').each(function(){
        $(this).text(js_beautify($(this).text(),
                                 { indent_size: 2}));
    });
    hljs.tabReplace = '    ';
    hljs.initHighlightingOnLoad();
    // Creates tabs from the code samples.
    // $('.example').each(function(){
    //     var example = $(this);
    //     var tabs = $('<div></div>');
    //     example.children('.lang').each(function(){
    //         var title = $(this).clone().click(function(){
    //             example.find('.pre').hide();
    //             pre.show();
    //             tabs.children().removeClass('tab-title-current');
    //             title.addClass('tab-title-current');
    //         });
    //         title.addClass('tab-title');
    //         var pre = $(this).next();
    //         tabs.append(title);
    //     }).remove();
    //     example.prepend(tabs);
    //     example.find('.pre').hide().first().show();
    //     tabs.children().first().addClass('tab-title-current');
    // });
    $('.example').each(function(){
        var tr = $(this).find('tr');
        var left = tr.find('td').first();
        var right = left.next();
    });
});
