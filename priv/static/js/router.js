Habanero.Router.map(function () {
    this.resource('habanero', { path: '/' });
    this.resource('past');
});

Habanero.HabaneroRoute = Ember.Route.extend({
    model: function () {
        return this.store.find('history');
    }
});