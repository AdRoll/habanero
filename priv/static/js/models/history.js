var attr = DS.attr;

Habanero.History = DS.Model.extend({
    date: attr(),
    duration: attr(),
    source: attr(),
    target: attr()
});

Habanero.History.FIXTURES = [
    {
        id: 1,
        date: "1379464272",
        duration: 330,
        source: "",
        target: ""
    },
    {
        id: 2,
        date: "1379463928",
        duration: 330,
        source: "",
        target: ""
    },
    {
        id: 3,
        date: "1379463301",
        duration: 330,
        source: "",
        target: ""
    }
];