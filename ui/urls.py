from django.urls import path
from . import views

urlpatterns = [
    path('', views.index, name='home'),
    path('api/metadata/', views.metadata, name='metadata'),
    path('api/default-settings/', views.default_settings, name='default-settings'),
    path('optimize/', views.optimize, name='optimize'),

    # Session (auth state for front-end)
    path('api/session/', views.api_session, name='api-session'),

    # Saved settings API (scoped to current user)
    path('api/saved-settings/', views.saved_settings_collection, name='saved-settings'),
    path('api/saved-settings/load', views.load_settings_view, name='saved-settings-load'),
    path('api/saved-settings/<int:pk>/', views.delete_saved_setting, name='saved-settings-detail'),  # DELETE
    path('api/saved-settings/delete', views.delete_saved_setting, name='saved-settings-delete'),     # POST fallback
    path('accounts/signup/', views.signup, name='signup'),
]
