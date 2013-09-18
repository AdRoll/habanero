var attr = DS.attr;

Habanero.History = DS.Model.extend({
    date: function () {
        return new Date(parseInt(this.get('id')) * 1000);
    }.property('id'),
    duration: attr(),
    source: attr(),
    target: attr()
});

Habanero.History.FIXTURES = [
    {
        id: "1379464272",
        duration: 330,
        source: "",
        target: ""
    },
    {
        id: "1379463928",
        duration: 330,
        source: "",
        target: ""
    },
    {
        id: "1379463301",
        duration: 330,
        source: "",
        target: ""
    }
];