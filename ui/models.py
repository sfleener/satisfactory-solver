from django.db import models
from django.contrib.auth.models import User

class DefaultSettings(models.Model):
    resource_limits = models.JSONField(blank=True, null=True, default=None)
    weights = models.JSONField(blank=True, null=True, default=None)
    recipes_off = models.JSONField(blank=True, null=True, default=None)
    inputs = models.JSONField(blank=True, null=True, default=None)
    outputs = models.JSONField(blank=True, null=True, default=None)
    max_item = models.CharField(max_length=128, blank=True, null=True)
    checkbox_Nuclear_Waste = models.BooleanField(default=False)
    integer_recipes = models.BooleanField(default=True)
    updated_at = models.DateTimeField(auto_now=True)

    class Meta:
        verbose_name = "Default settings"
        verbose_name_plural = "Default settings"

    def __str__(self):
        return f"DefaultSettings #{self.id} (updated {self.updated_at:%Y-%m-%d %H:%M})"

class SavedSettings(models.Model):
    owner = models.ForeignKey(User, on_delete=models.CASCADE, related_name='satopt_settings')
    name = models.CharField(max_length=200)
    settings = models.JSONField()
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    class Meta:
        unique_together = (('owner', 'name'),)
        ordering = ['-updated_at']
        verbose_name = "Saved settings"
        verbose_name_plural = "Saved settings"

    def __str__(self):
        return f"{self.owner.username}: {self.name}"