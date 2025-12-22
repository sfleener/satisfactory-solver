from django.contrib import admin
from .models import DefaultSettings, SavedSettings

@admin.register(DefaultSettings)
class DefaultSettingsAdmin(admin.ModelAdmin):
    list_display = ("id", "updated_at", "checkbox_Nuclear_Waste")
    readonly_fields = ("updated_at",)

@admin.register(SavedSettings)
class SavedSettingsAdmin(admin.ModelAdmin):
    list_display = ("name", "owner", "updated_at")
    search_fields = ("name", "owner__username")
    readonly_fields = ("updated_at",)
