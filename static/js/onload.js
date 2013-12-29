$(document).ready(function() {

    // Configure the navigation tabs
    $('#content').tabs({
        fxSlide: false,
        fxFade: true,
        fxSpeed: 'fast',
        onClick: function(newTab, content, oldTab) {
            if ($(newTab).hasClass('blog-redirect')){
                window.location.href = '/blog';
                return false;
            }
        }
    });

    // Activate Tipsy tooltip effect
    //$('#buy, #phone a').tipsy({
    //    gravity: 's',
    //    fade: true,
    //    html: true,
    //    // Text on the buy button tooltip:
    //    fallback: "Get it on Google Play"
    //});

    // Wrap the headers in the sidebar in span tags for styling purposes
    $('#sidebar h3').wrapInner('<span>');

    // Form validation in the contact form
    $("#contactForm").validate({
        errorElement: "em"
    });

    // Form validation in the newsletter form
    $("#newsletter").validate({
        errorElement: "em"
    });

    // Clear every fourth screenshot to keep the layout intact
    $("ul.screenshots li:nth-child(3n)").addClass('last');

    // Activate and configure the FancyBox lightbox plugin
    $(".fancybox").fancybox({
        openEffect  : 'elastic',
        closeEffect    : 'elastic',
        helpers:  {
            title : {
                type : 'inside'
            }
        }
    });

    // FancyBox lightbox plugin for videos
    $(".video").fancybox({
        maxWidth    : 800,
        maxHeight    : 600,
        fitToView    : false,
        width        : '70%',
        height        : '70%',
        autoSize    : false,
        closeClick    : false,
        openEffect    : 'none',
        closeEffect    : 'none',
        padding : 10
    });
});
