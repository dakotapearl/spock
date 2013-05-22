package spock.dataaccess.ejb.support.collections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.entities.Metric;
import spock.dataaccess.ejb.interfaces.entities.Network;
import spock.dataaccess.ejb.support.AbstractBasicEntityCollection;
import spock.dataaccess.entities.MetricEntity;
import javax.ejb.Stateful;

/**
 *
 * @author Loren Chorley
 */
//@Stateful
public class NetworkMetricCollection extends AbstractBasicEntityCollection<Network, Metric, String> {

    public NetworkMetricCollection() {
    }

    @Override
    protected Collection<Metric> getCollection(Network container) {
        return container.getMetrics();
    }

    @Override
    protected Metric newCollectionEntity(String id) {
        Metric x = new MetricEntity();
        x.setId(id);
        return x;
    }

    @Override
    protected void copyCollectionEntityProperties(Metric sourceEntity, Metric targetEntity) {
        targetEntity.setCodeURL(sourceEntity.getCodeURL());
    }

    @Override
    protected String getCollectionEntityID(Metric entity) {
        return entity.getId();
    }

    @Override
    protected Class getCollectionEntityClass() {
        return MetricEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(Metric entity) {
        
    }

}
