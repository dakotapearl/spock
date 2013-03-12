package spockdataaccess.ejb.requestsupport.entitycollections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spockdataaccess.ejb.requestsupport.generalisations.BasicEntityCollection;
import spockdataaccess.entity.Network;
import spockdataaccess.entity.Metric;

/**
 *
 * @author Loren Chorley
 */
public class NetworkMetricCollection extends BasicEntityCollection<Network, Metric, String> {

    public NetworkMetricCollection(EntityManager em) {
        super(em);
    }

    @Override
    protected Collection<Metric> getCollection(Network container) {
        return container.getMetrics();
    }

    @Override
    protected Metric newCollectionEntity(String id) {
        Metric x = new Metric();
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
        return Metric.class;
    }

    @Override
    protected void verifyBusinessLogic(Metric entity) {
        
    }

}
