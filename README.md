xml-slurp
=========

A group of libraries for web scraping &amp; crawling in chicken scheme. For example, the feed parser below:


    (define *feed-schema*
      '(?take-all
        (?or "item" "entry")
        (item
         (title          (?text "title"))
         (link           (?or
    		      (?tag ("link" (("rel" . "replies") ("type" . "text/html")))
    			    (?attr "href"))
    		      (?text "link")))
         (comments-link  (?or
    		      (?tag ("link" (("rel" . "replies") ("type" . "text/html")))
    			    (?attr "href"))
    		      (?text "comments")))
         (comments-count (?text "thr:total"))
         (date           (?date (?or "pubDate" "published")))
         (date-updated   (?date "atom:updated"))
         (author         (?or
    		      (?tag "author" (?text "name"))
    		      (?text (?or "author" "dc:creator"))))
         (body           (?text (?or "content" "content:encoded" "description")))
         (feeds
          (comments      (?or
    		      (?tag ("link" (("rel" . "replies") ("type" . "application/atom+xml")))
    			    (?attr "href"))
    		      (?text "wfw:commentRss"))))
         (media          (?take-all ("media:content" (("medium" . "image")))
    				(image (?attr "url")))))))
    
    (define (parse-feed feed)
      (xml:slurp (parse-xml feed) *feed-schema*))
