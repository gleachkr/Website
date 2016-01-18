window.addEvent('domready', function(){
    var scrollContent = new Fx.Scroll("content",{offset:{y:0}});
    var scrollBody = new Fx.Scroll("body",{offset:{y:0}});
    var scrollWindow = new Fx.Scroll(window);
    $$("#navBar div").set('morph', {
            duration:100, transition:'quad'
            });
    $$("#navBar a").forEach(function(elt){
        elt.removeAttribute("href");
    });
    $('Research').addEvent('click', function() {
        scrollContent.toElement("RecentResearch",'y');
        scrollBody.toElement("RecentResearch",'y');
        //alert("clicked!"); -- remove comments for testing
        });
    $('Teaching').addEvent('click', function() {
        scrollContent.toElement("RecentTeaching",'y');
        scrollBody.toElement("RecentTeaching",'y');
        //alert("clicked!"); -- remove comments for testing
        });
    $('AboutMe').addEvent('click', function() {
        scrollContent.toElement("RecentMe",'y');
        scrollBody.toElement("RecentMe",'y');
        //alert("clicked!"); -- remove comments for testing
        });
    $$("#navBar div").addEvents({
        mouseenter: function() { this.morph({ 'left':5 }); },
        mouseleave: function() { this.morph({ 'left':0 }); },
        click: function() { this.morph({ 'left':0 }); }
    });
});
