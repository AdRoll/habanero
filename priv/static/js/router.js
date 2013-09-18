Habanero.Router.map(function () {
    this.resource('habanero', { path: '/' });
    this.resource('history', { path: '/history/:id' });
});

Habanero.HabaneroRoute = Ember.Route.extend({
    model: function () {
        return {history: this.store.find('history')};
    }
});
