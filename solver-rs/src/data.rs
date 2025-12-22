use crate::rational::units::{Items, Machines, Megawatts, Per, Points, Recipes, Second};
use crate::rational::{ItemsPerMinute, ItemsPerMinutePerRecipe, Rat};
use lasso::Spur;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Display, Formatter};
use std::marker::PhantomData;
use std::sync::Arc;

thread_local! {
    static INTERNER: RefCell<lasso::Rodeo> = RefCell::new(lasso::Rodeo::new());
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum RecipeKind {}
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum ItemKind {}
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum MachineKind {}

pub type RecipeKey = Key<RecipeKind>;
pub type ItemKey = Key<ItemKind>;
pub type MachineKey = Key<MachineKind>;

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Key<T>(Spur, PhantomData<fn() -> T>);

impl<T> Serialize for Key<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        INTERNER.with(|i| serializer.serialize_str(i.borrow().resolve(&self.0)))
    }
}

impl<'de, T> Deserialize<'de> for Key<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;

        Ok(s.into())
    }
}

impl<T> Key<T> {
    pub fn new_static(s: &'static str) -> Self {
        Self(
            INTERNER.with(|l| l.borrow_mut().get_or_intern_static(s)),
            PhantomData,
        )
    }
}

impl<T> Debug for Key<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl<T> Display for Key<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        INTERNER.with(|l| f.write_str(l.borrow().resolve(&self.0)))
    }
}

impl<T> From<String> for Key<T> {
    fn from(value: String) -> Self {
        Self(
            INTERNER.with(|l| l.borrow_mut().get_or_intern(value)),
            PhantomData,
        )
    }
}

impl<T> From<&str> for Key<T> {
    fn from(value: &str) -> Self {
        Self(
            INTERNER.with(|l| l.borrow_mut().get_or_intern(value)),
            PhantomData,
        )
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct Settings {
    pub phase: Option<u8>,
    pub resource_limits: BTreeMap<ItemKey, ItemsPerMinute>,
    pub weights: Weights,
    pub recipes_off: BTreeSet<RecipeKey>,
    pub floating_recipes: BTreeSet<RecipeKey>,
    pub inputs: BTreeMap<ItemKey, ItemsPerMinute>,
    pub outputs: BTreeMap<ItemKey, ItemsPerMinute>,
    pub extras: BTreeMap<ItemKey, ItemsPerMinute>,
    pub max_item: Option<serde_json::Value>,
    #[serde(rename = "checkbox_Nuclear Waste")]
    pub force_nuclear_waste: bool,
    pub integer_recipes: bool,
}

impl Settings {
    pub fn floor_resource_limits(&mut self, floor: ItemsPerMinute) {
        for v in self.resource_limits.values_mut() {
            if *v < floor {
                *v = floor;
            }
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct Weights {
    #[serde(rename = "Power Use")]
    pub power_use: f64,
    #[serde(rename = "Item Use")]
    pub item_use: f64,
    #[serde(rename = "Building Use")]
    pub building_use: f64,
    #[serde(rename = "Resource Use")]
    pub resource_use: f64,
    #[serde(rename = "Buildings Scaled")]
    pub buildings_scaled: f64,
    #[serde(rename = "Resources Scaled")]
    pub resources_scaled: f64,
    #[serde(rename = "Nuclear Waste")]
    pub nuclear_waste: f64,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Data {
    pub items: BTreeMap<ItemKey, Arc<Item>>,
    pub resources: BTreeMap<ItemKey, Arc<Resource>>,
    pub recipes: BTreeMap<RecipeKey, Arc<Recipe>>,
    pub machines: BTreeMap<MachineKey, Arc<Machine>>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct Item {
    pub name: String,
    pub energy: Option<Rat<Megawatts>>,
    pub form: Option<Form>,
    pub points: Rat<Points>,
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone, Eq, PartialEq)]
#[serde(deny_unknown_fields)]
pub enum Form {
    #[serde(rename = "RF_SOLID")]
    Solid,
    #[serde(rename = "RF_LIQUID")]
    Liquid,
    #[serde(rename = "RF_GAS")]
    Gas,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct Resource {
    pub name: String,
    pub energy: Rat<Megawatts>,
    pub form: Form,
    pub points: Rat<Points>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(deny_unknown_fields)]
struct RawRecipe {
    pub name: String,
    pub time: Rat<Per<Second, Recipes>>,
    pub ingredients: Vec<RawRecipeItem>,
    pub products: Vec<RawRecipeItem>,
    pub machine: MachineKey,
    pub power_use: Rat<Per<Megawatts, Machines>>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
struct RawRecipeItem {
    pub item: ItemKey,
    pub amount: Rat<Items>,
}

#[derive(Debug)]
pub struct Recipe {
    raw: RawRecipe,
    pub name: String,
    pub ingredients: Vec<RecipeItem>,
    pub products: Vec<RecipeItem>,
    pub machine: MachineKey,
    pub power_use: Rat<Per<Megawatts, Machines>>,
}

impl Serialize for Recipe {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.raw.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Recipe {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = RawRecipe::deserialize(deserializer)?;

        let time = raw.time.recip().to_per_minute().recip();

        Ok(Recipe {
            name: raw.name.clone(),
            ingredients: raw
                .ingredients
                .iter()
                .map(|i| RecipeItem {
                    item: i.item,
                    amount: i.amount / time,
                })
                .collect(),
            products: raw
                .products
                .iter()
                .map(|i| RecipeItem {
                    item: i.item,
                    amount: i.amount / time,
                })
                .collect(),
            machine: raw.machine,
            power_use: raw.power_use,
            raw,
        })
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct RecipeItem {
    pub item: ItemKey,
    pub amount: ItemsPerMinutePerRecipe,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct Machine {
    pub name: String,
    pub power_use: f64,
    pub power_produced: f64,
}
