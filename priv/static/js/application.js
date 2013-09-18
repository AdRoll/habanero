window.Habanero = Ember.Application.create();

//Habanero.ApplicationAdapter = DS.FixtureAdapter.extend();

//Habanero.HabaneroAdapter = DS.RESTAdapter.extend({
//    namespace: '/api'
//});

Habanero.store = DS.Store.create({
    adapter: DS.RESTAdapter.create({
        namespace: 'superadmin'
    })
});

//Habanero.Store = DS.Store.extend({
//    adapter: DS.RESTAdapter.extend({
//        namespace: '/api'
//    })
//});

//Habanero.ApplicationAdapter = DS.RESTAdapter.extend({
//    namespace: function () {
//        return '/api/v1';
//    }
//});

