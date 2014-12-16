
type borderColor =
    None 
  | Red
  | Green

type t = {
    mAlgorithmCount  : int;
    mThumbnailSize   : int;
    mFileName        : string;
    mHtml            : out_channel;
}

let make aFileName aAlgorithmCount = {
  (* Most browsers will cap number of columns to real value,
   * so having something significantly larger works ok *)
  mAlgorithmCount = aAlgorithmCount;
  mThumbnailSize  = 128;
  mFileName = aFileName;
  mHtml = open_out aFileName;
}


let close doc =
  output_string doc.mHtml "</body>\n";
  output_string doc.mHtml "</html>\n"

(*
 * The Javascript plugin is heavily modified version of plugin example:
 * http://www.htmldrive.net/items/show/838/jQuery-different-Photo-comparing-Effect
 *
 * Original copyright reads:
 * // Queness Before & After jQuery Plugin
 * // Created by Kevin Liew from Queness.com
 * // Permission is given to use this plugin in whatever way you want :)
 *)
let header = "<!--
        * The Javascript plugin is heavily modified version of plugin example:
        * http://www.htmldrive.net/items/show/838/jQuery-different-Photo-comparing-Effect
        *
        * Original copyright reads:
        * // Queness Before & After jQuery Plugin
        * // Created by Kevin Liew from Queness.com
        * // Permission is given to use this plugin in whatever way you want :)
        -->
        <!DOCTYPE html PUBLIC 
            '-//W3C//DTD XHTML 1.0 Strict//EN'
            'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>
        <html xmlns='http://www.w3.org/1999/xhtml'>
        <head>
        <title>Comparison of GI algorithms with Vertex Connection Merging</title>
        <meta http-equiv='Content-Language' content='English' />
        <meta http-equiv='Content-Type' content='text/html; charset=UTF-8' />
        <script type='text/javascript' src='http://ajax.googleapis.com/ajax/libs/jquery/1.4.4/jquery.min.js'></script>
        <script type='text/javascript'>
        <!--
            (function($){
            $.fn.extend({
                //plugin name - qbeforeafter
                qbeforeafter: function(options) {
        
                    return this.each(function() {
        
                        var i = $(this);
                        var img_layer_tl = i.children('img:eq(0)').attr('src');
                        var img_layer_tr = i.children('img:eq(1)').attr('src');
                        var img_layer_bl = i.children('img:eq(2)').attr('src');
                        var img_layer_br = i.children('img:eq(3)').attr('src');
                        var img_cap_tl = i.children('img:eq(0)').attr('alt');
                        var img_cap_tr = i.children('img:eq(1)').attr('alt');
                        var img_cap_bl = i.children('img:eq(2)').attr('alt');
                        var img_cap_br = i.children('img:eq(3)').attr('alt');
        
                        var img_style_tl = i.children('img:eq(0)').attr('style');
                        var img_style_tr = i.children('img:eq(1)').attr('style');
                        var img_style_bl = i.children('img:eq(2)').attr('style');
                        var img_style_br = i.children('img:eq(3)').attr('style');
        
                        var width = i.children('img:eq(0)').width();
                        var height = i.children('img:eq(0)').height();
        
                        i.children('img').hide();
        
                        i.css({'overflow': 'hidden', 'position': 'relative'});
                        i.append('<div class='ba-layer_tl'></div>');
                        i.append('<div class='ba-layer_tr'></div>');
                        i.append('<div class='ba-layer_bl'></div>');
                        i.append('<div class='ba-layer_br'></div>');
        
                        i.append('<div class='ba-border_tl' style='' + img_style_tl + ''></div>');
                        i.append('<div class='ba-border_tr' style='' + img_style_tr + ''></div>');
                        i.append('<div class='ba-border_bl' style='' + img_style_bl + ''></div>');
                        i.append('<div class='ba-border_br' style='' + img_style_br + ''></div>');
        
                        i.append('<div class='ba-caption_tl'>' + img_cap_tl + '</div>');
                        i.append('<div class='ba-caption_tr'>' + img_cap_tr + '</div>');
                        i.append('<div class='ba-caption_bl'>' + img_cap_bl + '</div>');
                        i.append('<div class='ba-caption_br'>' + img_cap_br + '</div>');
        
                        i.children('.ba-layer_tl, .ba-layer_tr, .ba-layer_bl, .ba-layer_br').width(width);
                        i.children('.ba-layer_tl, .ba-layer_tr, .ba-layer_bl, .ba-layer_br').height(height);
        
                        i.children('.ba-layer_tl').css('backgroundImage','url(' + img_layer_tl + ')');
                        i.children('.ba-layer_tr').css('backgroundImage','url(' + img_layer_tr + ')');
                        i.children('.ba-layer_bl').css('backgroundImage','url(' + img_layer_bl + ')');
                        i.children('.ba-layer_br').css('backgroundImage','url(' + img_layer_br + ')');
        
                        i.children('.ba-layer_tl').width(width * 0.5);
                        i.children('.ba-layer_tl').height(height * 0.5);
                        i.children('.ba-layer_tr').height(height * 0.5);
                        i.children('.ba-layer_bl').width(width * 0.5);
        
                        i.children('.ba-caption_tl').show();
                        i.children('.ba-caption_tr').show();
                        i.children('.ba-caption_bl').show();
                        i.children('.ba-caption_br').show();
        
                        i.children('.ba-caption_tl').css({ bottom: height * 0.5, right: width * 0.5 });
                        i.children('.ba-caption_tr').css({ bottom: height * 0.5, left:  width * 0.5 });
                        i.children('.ba-caption_bl').css({ top:    height * 0.5, right: width * 0.5 });
                        i.children('.ba-caption_br').css({ top:    height * 0.5, left:  width * 0.5 });
                        // Set border
                        bwidth = parseInt(i.children('.ba-border_tl').css('borderRightWidth'), 10);
                        i.children('.ba-border_tl').width (width  * 0.5 - 1 - 2*(bwidth-1));
                        i.children('.ba-border_tl').height(height * 0.5 - 1 - 2*(bwidth-1));
                        i.children('.ba-border_tr').width (width  * 0.5 - 4 - 2*(bwidth-1));
                        i.children('.ba-border_tr').height(height * 0.5 - 1 - 2*(bwidth-1));
                        i.children('.ba-border_bl').width (width  * 0.5 - 1 - 2*(bwidth-1));
                        i.children('.ba-border_bl').height(height * 0.5 - 4 - 2*(bwidth-1));
                        i.children('.ba-border_br').width (width  * 0.5 - 4 - 2*(bwidth-1));
                        i.children('.ba-border_br').height(height * 0.5 - 4 - 2*(bwidth-1));
        
                        i.children('.ba-border_tl').css({ top: 0,                left: 0              });
                        i.children('.ba-border_tr').css({ top: 0,                left: width * 0.5 + 2});
                        i.children('.ba-border_bl').css({ top: height * 0.5 + 2, left: 0              });
                        i.children('.ba-border_br').css({ top: height * 0.5 + 2, left: width * 0.5 + 2});
        
                    }).mousemove(function (e) {
        
                        var o = options;
                        var i = $(this);
        
                        right_border_width = parseInt(i.children('.ba-layer_tl').css('borderRightWidth'), 10);
                        bottom_border_width = parseInt(i.children('.ba-layer_tl').css('borderBottomWidth'), 10);
                        pos_imgX = i.position()['left'];
                        pos_imgY = i.position()['top'];
                        pos_mouseX = e.pageX - right_border_width * 0.5;
                        pos_mouseY = e.pageY - bottom_border_width * 0.5;
                        new_lwidth  = pos_mouseX - pos_imgX; // left width
                        new_theight = pos_mouseY - pos_imgY; // top height
                        img_width   = i.width();
                        img_height  = i.height();
                        new_rwidth  = img_width  - new_lwidth;  // right width
                        new_bheight = img_height - new_theight; // bottom height
        
                        img_cap_tl = i.children('img:eq(0)').attr('alt');
                        img_cap_tr = i.children('img:eq(1)').attr('alt');
                        img_cap_bl = i.children('img:eq(2)').attr('alt');
                        img_cap_br = i.children('img:eq(3)').attr('alt');
        
                        i.children('.ba-layer_tl').width (new_lwidth );
                        i.children('.ba-layer_tl').height(new_theight);
                        i.children('.ba-layer_tr').height(new_theight);
                        i.children('.ba-layer_bl').width (new_lwidth );
        
                        i.children('.ba-caption_tl').css({ bottom: new_bheight, right: new_rwidth });
                        i.children('.ba-caption_tr').css({ bottom: new_bheight, left:  new_lwidth });
                        i.children('.ba-caption_bl').css({ top:    new_theight, right: new_rwidth });
                        i.children('.ba-caption_br').css({ top:    new_theight, left:  new_lwidth });
                        // Set border
                        bwidth = parseInt(i.children('.ba-border_tl').css('borderRightWidth'), 10);
                        i.children('.ba-border_tl').width (new_lwidth  - 1 - 2*(bwidth-1));
                        i.children('.ba-border_tl').height(new_theight - 1 - 2*(bwidth-1));
                        i.children('.ba-border_tr').width (new_rwidth  - 4 - 2*(bwidth-1));
                        i.children('.ba-border_tr').height(new_theight - 1 - 2*(bwidth-1));
                        i.children('.ba-border_bl').width (new_lwidth  - 1 - 2*(bwidth-1));
                        i.children('.ba-border_bl').height(new_bheight - 4 - 2*(bwidth-1));
                        i.children('.ba-border_br').width (new_rwidth  - 4 - 2*(bwidth-1));
                        i.children('.ba-border_br').height(new_bheight - 4 - 2*(bwidth-1));
        
                        i.children('.ba-border_tl').css({ top: 0,               left: 0             });
                        i.children('.ba-border_tr').css({ top: 0,               left: new_lwidth + 2});
                        i.children('.ba-border_bl').css({ top: new_theight + 2, left: 0             });
                        i.children('.ba-border_br').css({ top: new_theight + 2, left: new_lwidth + 2});
                    });
                }
            });
            })(jQuery);
        -->
        </script>
        
        <script type='text/javascript'>
        $(function () {
          $('.cross_compare').qbeforeafter({defaultgap:50, leftgap:0, rightgap:10, caption: true, reveal: 0.5});
        });
        </script>
        <style type='text/css'>
        .ba-layer_tl {position:absolute; top:0; left:0; z-index:3; border-right:3px solid #333; border-bottom:3px solid #333;}
        .ba-layer_tr {position:absolute; top:0; left:0; z-index:2; border-bottom:3px solid #333;}
        .ba-layer_bl {position:absolute; top:0; left:0; z-index:1; border-right:3px solid #333;}
        .ba-layer_br {position:absolute; top:0; left:0; z-index:0;}
        
        .ba-border_tl {position:absolute; top:0; left:0; z-index:4;}
        .ba-border_tr {position:absolute; top:0; left:0; z-index:4;}
        .ba-border_bl {position:absolute; top:0; left:0; z-index:4;}
        .ba-border_br {position:absolute; top:0; left:0; z-index:4;}
        
        .ba-caption_tl {position:absolute; bottom:10px; right:10px; z-index:120; color:#fff; text-align:center; padding:5px; font-size:12px; font-family:arial; display:none;}
        .ba-caption_tr {position:absolute; bottom:10px; left: 10px; z-index:120; color:#fff; text-align:center; padding:5px; font-size:12px; font-family:arial; display:none;}
        .ba-caption_bl {position:absolute; top:10px;    right:10px; z-index:120; color:#fff; text-align:center; padding:5px; font-size:12px; font-family:arial; display:none;}
        .ba-caption_br {position:absolute; top:10px;    left: 10px; z-index:120; color:#fff; text-align:center; padding:5px; font-size:12px; font-family:arial; display:none;}
        </style>
        </head>
        <body>"
;;

let writeHeader doc =
  output_string doc.mHtml header

let (|<) doc str = 
  let () = output_string doc.mHtml str in doc

let (|<%) doc i =
  let () = output_string doc.mHtml (string_of_int i) in doc

let addScene doc aSceneName =
  doc |< "<table";

  if doc.mAlgorithmCount < 100
  then doc |< " width='" |<% doc.mAlgorithmCount * (doc.mThumbnailSize + 10) |< "'" else doc;

  doc |< "><tr><td colspan='" |<% doc.mAlgorithmCount |< "'><h2>" |< aSceneName |< "</h2></td></tr>\n";
  doc |< "<tr>\n"

let makeMessage fmt =
  Printf.sprintf fmt

let addRendering doc ?(aBorderColor = None) ?(aOtherInfo = "") aMethodName aFileName aTime =
        (* The image *)
  doc
  |< "<td valign='top' align='center'>"
  |< "<div style='"
  |< "width:"|<% doc.mThumbnailSize + 10 |<"px;line-height:90%;'>"
  |< " <a href='" |< aFileName |< "'>";
  if true
  then (
    doc |< "<img src='" |< aFileName |< "' "  |< "width='"|<% doc.mThumbnailSize |<"px'  ";
    doc |< (match aBorderColor with
              Green -> "style='border:5px solid #0c0' ";
            | Red   -> "style='border:5px solid #f00' ";
            | _     -> "style='border:5px solid #ccc' ";);
    doc |< " alt='" |< aFileName |< " (" |< (makeMessage "%.2f" aTime) |< " s)' ";
    doc |< "height='"|<% doc.mThumbnailSize |<"px' />";
  ) else (
    doc |< "<div style='background: url(" |< aFileName |< "); background-size: 128px;";
    match aBorderColor with
            | Red  ->
               doc |< "background-position: -5px -5px; border: 5px solid #f00; "
               |< "width:"|<% doc.mThumbnailSize - 10 |<"px; "
               |< "height:"|<% doc.mThumbnailSize - 10 |<"px;'></div>";

            | Green ->
               doc |< "background-position: -5px -5px; border: 5px solid #0c0; "
               |< "width:"|<% doc.mThumbnailSize - 10 |<"px; "
               |< "height:"|<% doc.mThumbnailSize - 10 |<"px;'></div>";
            | _ ->
               doc |< "width:"|<% doc.mThumbnailSize |<"px; "
               |< "height:"|<% doc.mThumbnailSize |<"px;'></div>";
  );
  doc |< "</a>\n";
  (* The text *)
  doc |< "<br/><small>" |< aMethodName
  |< " (" |< (makeMessage "%.2f" aTime) |< " s)" |< aOtherInfo
  |< "</small></div></td>\n"

let addFourWaySplit doc aMethodFilesBordersNames aSize =
  doc |< "</tr><tr>\n";
  doc |< "<td colspan='" |<% doc.mAlgorithmCount |< "' align='center'>\n";
  doc |< "<div class='cross_compare' style='width:" |<% aSize
  |< "px;height:" |<% aSize |< "px;cursor:crosshair'>\n";
  let doMethod = fun (file,border,acronym) doc ->
    doc |< "<img src='" |< file
    |< "' alt='" |< acronym
    |< "' width='" |<% aSize
    |< "' height='" |<% aSize |< "' ";
    doc |< (match border with
            | Green -> "style='border:2px solid #0c0'/>\n"
            | Red   -> "style='border:2px solid #f00'/>\n"
            | _     -> "style='border:2px solid #ccc'/>\n");
  in 
  List.fold_right doMethod aMethodFilesBordersNames doc
  |< "</div>\n"
  |< "</td>\n"
  |< "</tr></table>\n"
