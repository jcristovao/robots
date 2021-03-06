User-agent: *
Allow: /ads/public/
Disallow: /ads/
Disallow: /adx/bin/
Disallow: /aponline/
Disallow: /archives/
Disallow: /auth/
Disallow: /cnet/
Disallow: /college/
Disallow: /external/
Disallow: /financialtimes/
Disallow: /idg/
Disallow: /indexes/
Disallow: /library/
Disallow: /nytimes-partners/
Disallow: /packages/flash/multimedia/TEMPLATES/
Disallow: /pages/college/
Disallow: /paidcontent/
Disallow: /partners/
Disallow: /reuters/
Disallow: /thestreet/
Allow: /svc/news/v3/all/pshb.rss	
Disallow: /svc
Disallow: /video/*/multimedia/*

User-agent: Mediapartners-Google
Disallow:

User-agent: AdsBot-Google
Disallow:

User-agent: adidxbot
Disallow:

Sitemap: http://spiderbites.nytimes.com/sitemaps/www.nytimes.com/sitemap.xml.gz
Sitemap: http://www.nytimes.com/sitemaps/sitemap_news/sitemap.xml.gz
Sitemap: http://spiderbites.nytimes.com/sitemaps/sitemap_video/sitemap.xml.gz
Sitemap: http://spiderbites.nytimes.com/sitemaps/video.nytimes.com/sitemap.xml.gz