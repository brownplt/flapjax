from django.conf.urls.defaults import *
from django.conf import settings

urlpatterns = patterns('',
  (r'^fxserver/getobj/(?P<obj_id>\w+)/$','fxserver.views.getobj'),
  (r'^fxserver/getobj/(?P<obj_id>\w+)$','fxserver.views.getobj'),
  (r'^fxserver/setobj/(?P<obj_id>\w+)/$','fxserver.views.setobj'),
  (r'^fxserver/setobj/(?P<obj_id>\w+)$','fxserver.views.setobj'),

#     (r'^admin/', include('django.contrib.admin.urls')),
)


if False:
  urlpatterns += patterns('',
    (r'^(?P<path>.*)$', 'django.views.static.serve', 
     {'document_root': '../build', 'show_indexes': True}),
  )

